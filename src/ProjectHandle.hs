{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectHandle where

import Prelude hiding (id, writeFile, readFile)
import GHC.Generics (Generic)

import Control.Monad
import Control.DeepSeq

import Data.Data (Typeable, Data)
import Data.Int (Int64)
-- import Data.Text (Text)
import Data.Maybe (fromJust, isNothing)
import Data.List (intercalate, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Aeson (decode, encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (readFile, writeFile, ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (pack)

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO hiding (writeFile, readFile)
import System.Directory
import System.Directory.Tree
import System.FilePath.Windows

import Data.Time.Clock
import Data.Time.Calendar

import ProjectConfig
import VerilogHandle

globalConfigJson = ConfigData {
    author = "",
    contact = "",
    version  = Version {
        v0 = 0,
        v1 = 0,
        v2 = 0,
        v3 = 1
    },
    verstep  = Version {
        v0 = 0,
        v1 = 0,
        v2 = 0,
        v3 = 1
    }
}

allowedFileExtension = [".v",".sv",".vhd"]

writeJson path d = writeFile path $ toStrict $ encode d 

initProject :: [FilePath] -> FilePath -> IO()
initProject filepaths currentPath =
    if "-" `elem` filepaths then createProject currentPath
    else if length filepaths /= 1 then do
        hPutStrLn stderr "Too many arguments. Try --help\n"
        exitWith (ExitFailure 1)
    else createProject (head filepaths)

getDirPath :: String -> String
getDirPath path = intercalate "\\" (init (splitOn "\\" path))

createProject :: String -> IO()
createProject projectPath = do
    executePath <- getDirPath . fromJust <$> findExecutable "ccv.exe"
    let configFilePath = executePath </> ".global.project"
    let projectFilePath = projectPath </> ".ccv.project"
    -- putStrLn $ show ("The config file is " ++ configFilePath)
    -- putStrLn $ show ("The project file is " ++ projectFilePath)
    projectExist <- doesFileExist projectFilePath
    if projectExist
    then putStrLn $ "Reinitialized project in " ++ projectPath
    else putStrLn $ "Initialized project in " ++ projectPath
    checkConfigFile configFilePath projectFilePath projectPath

checkConfigFile :: FilePath -> FilePath -> FilePath -> IO()
checkConfigFile configFilePath projectFilePath projectPath= do
    fileExist <- doesFileExist configFilePath
    if fileExist then do
        maybeConfigData <- getConfigData
        if isNothing maybeConfigData then do
            removeFile configFilePath
            writeFile configFilePath $ toStrict $ encode globalConfigJson
            putStrLn "Reinitialized Configuration File."
            createProjectFile projectFilePath projectPath globalConfigJson
        else do
            let configData = fromJust maybeConfigData
            -- print configData
            -- print "Global configuration file is found."
            createProjectFile projectFilePath projectPath configData
    else do
        writeFile configFilePath $ toStrict $ encode globalConfigJson
        putStrLn "Reinitialized Configuration File."
        createProjectFile projectFilePath projectPath globalConfigJson

createProjectFileWithoutConfigData :: FilePath -> IO()
createProjectFileWithoutConfigData path = do
    executePath <- getDirPath . fromJust <$> findExecutable "ccv.exe"
    checkConfigFile (executePath </> ".global.project")
        path (getDirPath path)

createProjectFile :: String -> FilePath -> ConfigData -> IO()
createProjectFile filepath path configData = do
    projectExist <- doesFileExist filepath
    subDirTree <- dirTree <$> readDirectory path
    writeFile "dirtree.tmp" $ toStrict (pack (show subDirTree))
    removeFile "dirtree.tmp"
    when (not $ successful subDirTree) $
        error ("Fail to readDirectory")
    let direcTree = toDirecTree subDirTree
    let fileList = map (path ++) $ getFileList [] subDirTree
    verilogList <- sequence (parseFromFileList fileList)
    -- writeFile "module.py" $ pack (show (convertToModuleList fileList verilogList))
    let moduleUnitList = convertToModuleUnitList fileList verilogList
    let moduleList = convertToModuleList verilogList
    checkNameConflict moduleUnitList
    -- mconcat $ map (putStrLn . show) $ moduleUnitList
    -- when (projectExist) $ removeFile filepath
    let hierTree = convertToProjectTreeList moduleList
    let projectData = ProjectData direcTree hierTree moduleUnitList
    let topData = TopData configData projectData
    -- print "Already get all information."
    when projectExist $ removeFile filepath
    writeFile filepath $ toStrict $ encode topData

genTopWithVerilog :: [FilePath] -> FilePath -> [String] -> IO()
genTopWithVerilog (vFile:cfs) currentPath args = do
    let cyFiles = filter (isSuffixOf ".cy") cfs
    when (null cyFiles) $ error ("No cynide file specified.")
    let vName = dropExtension vFile
    projectPath <- getProjectPath currentPath
    projectFile <- readFile projectPath
    let projectData = decode $ fromStrict projectFile :: Maybe TopData
    let topData = getData projectData "Project"
    let moduleUnitList = getModuleList topData 
    let vFilePath = (getDirPath projectPath) </> (getVFilePath moduleUnitList vName)
    verilogData <- parseVerilogFile vFilePath
    cynideList <- sequence $ map getCynideFile $ map (currentPath </>) cyFiles
    let verilogList = map (convertCynideToVerilog moduleUnitList) cynideList
    renameFile vFilePath $ vFilePath ++ ".old"
    -- print vFilePath
    putStrLn $ "Add " ++ intercalate " " cyFiles ++ " into " ++ vFile
    if "Port" `elem` args then do
        printVerilog (addPortComment moduleUnitList $ mergeVerilog verilogData verilogList) vFilePath
    else do
        printVerilog (mergeVerilog verilogData verilogList) vFilePath

genTopFromFileList :: [FilePath] -> FilePath -> [String] -> IO()
genTopFromFileList filepaths currentPath args = do 
    let cyFiles = filter (isSuffixOf ".cy") filepaths
    if null cyFiles then
        error ("No cynide file specified.")
    else
        putStrLn  ("Ready to convert:" ++ intercalate " " filepaths)
    projectPath <- getProjectPath currentPath
    projectFile <- readFile projectPath
    let projectData = decode $ fromStrict projectFile :: Maybe TopData
    let topData = getData projectData "Project"
    let moduleUnitList = getModuleList topData
    mconcat $ map (genTopFromFile currentPath moduleUnitList args) cyFiles

genTopFromFile :: FilePath -> [ModuleUnit] -> [String] -> FilePath -> IO ()
genTopFromFile currentPath mods args cyName  = do
    let cyFile = currentPath </> cyName
    cyData <- getCynideFile cyFile
    -- print cyData 
    -- print mods
    let verilogData = convertCynideToVerilog mods cyData 
    let verilogFileName = addExtension (dropExtension cyFile) "v"
    if "Port" `elem` args then do
        printVerilog (addPortComment mods verilogData ) verilogFileName
    else do
        printVerilog (verilogData) verilogFileName

getModuleList (TopData _ (ProjectData _ _ ms)) = ms

getData Nothing  s = error $ s ++ " file corrupted. Try ccv --init or ccv --update"
getData (Just x) _ = x

getProjectPath :: FilePath -> IO (FilePath)
getProjectPath path = do filePath <- getFilePath $ (splitOn "\\" path)
                         return filePath
getFilePath p = do  fileExist <- doesFileExist $ (intercalate "\\" p) </> ".ccv.project"
                    case fileExist of
                        True -> return $ (intercalate "\\" p) </> ".ccv.project"
                        False -> case (length p) == 0 of 
                            True -> error ("Project File is not found. Init project first. (ccv --init)")
                            False -> getFilePath $ init p

checkNameConflict :: [ModuleUnit] -> IO()
checkNameConflict [] = return ()
checkNameConflict x = mconcat (map (checkName x) x) where
    checkName list unit = mconcat (map (compareName unit) list)
    compareName (ModuleUnit na pa _ _ _) (ModuleUnit nb pb _ _ _) = 
        case and [(na == nb), (pa /= pb)] of 
            True -> putStrLn $ "Name conflict: " ++ na ++ "::" ++ pa ++ " == " ++ nb ++ "::" ++ pb
            False -> return ()

toDirecTree :: DirTree String -> DirecTree
toDirecTree (File name file) = DirecTree name []
toDirecTree (Dir  name dir)  = DirecTree name $ map toDirecTree dir

getDirFileName :: [String] -> [String] -> DirTree String -> [String]
getDirFileName path fileList (File name _)   = (intercalate "\\" (path ++ [name])) : fileList
getDirFileName path fileList (Dir  name dir) = concat $ map (getDirFileName (path ++ [name]) fileList) dir

getFileList :: [String] -> DirTree String -> [String]
getFileList _ (Dir _ []) = []
getFileList path (Dir _ dir) = filter (multSuffix allowedFileExtension) $ getDirFileName path [] (Dir "" dir)
getFileList _ _ = []

multSuffix :: [String] -> String -> Bool
multSuffix cond str = or $ map ($ str) (map isSuffixOf cond)

setEmail :: [String] -> String -> IO ()
setEmail [] _ = error "No email specified. ccv --email email"
setEmail emails currentPath = do 
    when (head emails == "-") $  error "No email specified. ccv --email email"
    executePath <- getDirPath . fromJust <$> findExecutable "ccv.exe"
    let configFilePath = executePath </> ".global.project"
    configData <- getConfigData
    projectPath <- getProjectPath currentPath
    projectData <- getProjectData projectPath
    let configData0 = getData configData "config"
    let configData1 = setEmailConfig configData0 $ head emails
    let topData = getData projectData "project"
    let topData1 = setEmailProject topData $ head emails
    removeFile $ configFilePath
    removeFile $ projectPath
    writeJson configFilePath configData1
    writeJson projectPath topData1
    putStrLn $ "The email has been set to " ++ (head emails)

setAuthor :: [String] -> String -> IO ()
setAuthor [] _ = error "No author specified. ccv --author author"
setAuthor authors currentPath = do 
    when (head authors == "-") $  error "No author specified. ccv --author author"
    executePath <- getDirPath . fromJust <$> findExecutable "ccv.exe"
    let configFilePath = executePath </> ".global.project"
    configData <- getConfigData
    projectPath <- getProjectPath currentPath
    projectData <- getProjectData projectPath
    let configData0 = getData configData "config"
    let configData1 = setAuthorConfig configData0 $ head authors
    let topData = getData projectData "project"
    let topData1 = setAuthorProject topData $ head authors
    removeFile $ configFilePath
    removeFile $ projectPath
    writeJson configFilePath configData1
    writeJson projectPath topData1
    putStrLn $ "The author has been set to " ++ (head authors)

setEmailConfig (ConfigData au _ v1 v2) s = ConfigData au s v1 v2
setEmailProject (TopData cfd pj) s = TopData (setEmailConfig cfd s) pj
setAuthorConfig (ConfigData _ s v1 v2) au = ConfigData au s v1 v2
setAuthorProject (TopData cfd pj) s = TopData (setAuthorConfig cfd s) pj

getConfigData :: IO (Maybe ConfigData)
getConfigData = do 
    executePath <- getDirPath . fromJust <$> findExecutable "ccv"
    let configFilePath = executePath </> ".global.project"
    configFile <- readFile configFilePath
    let configData = decode $ fromStrict configFile :: Maybe ConfigData
    return configData

getProjectData :: FilePath -> IO (Maybe TopData)
getProjectData path = do
    pjFile <- readFile path
    let topData = decode $ fromStrict pjFile :: Maybe TopData
    return topData

updateProject :: FilePath -> IO ()
updateProject path = do
    projectFilePath <- getProjectPath path
    putStrLn $ "find project root in " ++ projectFilePath
    createProjectFileWithoutConfigData projectFilePath

newVerilogFiles :: [FilePath] -> FilePath -> IO ()
newVerilogFiles files currentPath = do 
    when (null files) $ error ("Unknown error.")
    when ((head files) == "-") $ error ("No file specified.")
    configData0 <- getConfigData
    let configData = getData configData0 "config"
    dateString <- date
    mconcat $ map (newVerilogFile currentPath dateString configData) files

-- foreach f list = forM list (\i -> do f i)

newVerilogFile :: FilePath -> String -> ConfigData -> FilePath -> IO()
newVerilogFile currentPath dateString configData fileName = do
    let verilogData = newVerilogData fileName
    putStrLn $ "Generated " ++ vFile
    printVerilogWithHeader verilogData fileName (currentPath </> vFile) vFile configData dateString
    where vFile = (addExtension (dropExtension fileName) "v")

date :: IO String
date = getCurrentTime >>= return . renderDate . toGregorian . utctDay
    where renderDate (y, m, d) = intercalate "/" $ [(show y),(show m),(show d)]

mergeMultiFiles :: [FilePath] -> FilePath -> [String] -> IO ()
mergeMultiFiles paths currentPath args = do
    let filePaths = filter (isSuffixOf ".v") paths
    when (null filePaths) $ error ("No verilog file specified. Add extension .v")
    when ((length filePaths) == 1) (error "Only one file specified")
    putStrLn $ intercalate " " (tail filePaths) ++ be (tail filePaths) ++ "merged into " ++ head filePaths 
    verilogList <- sequence (parseFromFileList filePaths)
    renameMultiFile currentPath [head filePaths] ".old"
    if "Increment" `elem` args then
        printVerilog (mergeVerilog (head verilogList) (tail verilogList)) $ head filePaths
    else
        printVerilog (mergeVerilogFile verilogList) $ head filePaths
    where
        be s = case length s of 
            1 -> " is "
            2 -> " are "

splitMultiFiles :: [FilePath] -> FilePath -> IO ()
splitMultiFiles paths currentPath = do
    let filePaths = filter (isSuffixOf ".v") paths
    when (null filePaths) $ error ("No verilog file specified. Add extension .v")
    verilogList <- sequence (parseFromFileList (map (currentPath </>) paths))
    renameMultiFile currentPath filePaths ".old"
    mconcat $ map splitSingleFile verilogList
    where
        splitSingleFile vFile = do  
            putStrLn $ printFirstInfo (splitVerilogFile vFile)
            mconcat $ map (\(vdata, vpath) -> 
                printVerilog vdata (currentPath </> vpath ++ ".v")) (splitVerilogFile vFile)
            where
                printSplitInfos s = " is split into " ++ intercalate " " (map printSplitInfo s)
                printSplitInfo (_, vpath) =  vpath ++ ".v"
                printFirstInfo [] = error "Original file empty."
                printFirstInfo x = snd (head x) ++ ".v" ++ printSplitInfos x

renameMultiFile currentPath paths ex = 
    mconcat $ map renameFileTuple $ zip (map (currentPath </>) paths) (map (\s -> currentPath </> s ++ ex) paths)
    where renameFileTuple (oName,nName) = renameFile oName nName

printHierarchy :: FilePath -> IO ()
printHierarchy currentPath = do
    projectPath <- getProjectPath currentPath
    projectData <- getProjectData projectPath
    let topData = getData projectData "project"
    let hierList = hierarchy $ project_data topData
    putStrLn $ concat $ map (printProjectTree 0) hierList
    where
        printProjectTree d t = 
            replicate (d*4) ' ' ++ "|" ++ hier_name t ++ "::" ++ modu_name t ++ "\n" ++
            case hier_node t of
                [] -> ""
                _  -> concat $ map (printProjectTree $ d + 1) $ hier_node t

formatVerilog :: [FilePath] -> FilePath -> [String] -> IO ()
formatVerilog paths currentPath args = do
    let filePaths = filter (isSuffixOf ".v") paths
    when (null filePaths) $ error "No Verilog File specified. Add extension."
    verilogList <- sequence (parseFromFileList $ map (currentPath </>) filePaths)
    dateString <- date
    configData0 <- getConfigData
    projectPath <- getProjectPath currentPath
    projectData <- getProjectData projectPath
    let topData = getData projectData "project"
    let moduleUnitList = modules $ project_data topData
    let configData = getData configData0 "config"
    renameMultiFile currentPath filePaths ".old"
    if "Port" `elem` args then
        mconcat $ map (printMultiVerilog currentPath dateString configData) $ zip (map (addPortComment moduleUnitList) verilogList) filePaths
    else
        mconcat $ map (printMultiVerilog currentPath dateString configData) $ zip verilogList filePaths
    where
        printMultiVerilog currentPath dateString configData (vData, fileName)  = do
            putStrLn $ "Pretty print " ++ fileName
            printVerilogWithHeader vData (dropExtension fileName) (currentPath </> fileName) fileName configData dateString