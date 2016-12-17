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

import ProjectConfig
import VerilogHandle

globalConfigJson = ConfigData {
    author = "",
    contact = "",
    version  = Version {
        v0 = 0,
        v1 = 0,
        v2 = 0,
        v3 = 0
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
    putStrLn $ show ("The config file is " ++ configFilePath)
    putStrLn $ show ("The project file is " ++ projectFilePath)
    projectExist <- doesFileExist projectFilePath
    if projectExist
    then putStrLn $ "Reinitialized project in " ++ show projectPath
    else putStrLn $ "Initialized project in " ++ show projectPath
    checkConfigFile configFilePath projectFilePath projectPath

checkConfigFile :: FilePath -> FilePath -> FilePath -> IO()
checkConfigFile configFilePath projectFilePath projectPath= do
    fileExist <- doesFileExist configFilePath
    if fileExist then do
        maybeConfigData <- getConfigData configFilePath
        if isNothing maybeConfigData then do
            removeFile configFilePath
            writeFile configFilePath $ toStrict $ encode globalConfigJson
            print "Reinitialized Configuration File."
            createProjectFile projectFilePath projectPath globalConfigJson
        else do
            let configData = fromJust maybeConfigData
            -- print configData
            print "Global configuration file is found."
            createProjectFile projectFilePath projectPath configData
    else do
        writeFile configFilePath $ toStrict $ encode globalConfigJson
        print "Reinitialized Configuration File."
        createProjectFile projectFilePath projectPath globalConfigJson

createProjectFileWithoutConfigData :: FilePath -> IO()
createProjectFileWithoutConfigData path = do
    executePath <- getDirPath . fromJust <$> findExecutable "ccv.exe"
    checkConfigFile (executePath </> ".global.project")
        path (getDirPath path)

createProjectFile :: String -> FilePath -> ConfigData -> IO()
createProjectFile filepath path configData = do
    projectExist <- doesFileExist filepath
    when projectExist (removeFile filepath)
    subDirTree <- dirTree <$> readDirectory path
    -- writeFile "dirtree.txt" (pack (show subDirTree))
    if successful subDirTree then do
        print "Changing to file directory ..."
    else do
        print "Failed to read file directory."
        return ()
    let direcTree = toDirecTree subDirTree
    let fileList = map (path ++) $ getFileList [] subDirTree
    print fileList
    verilogList <- sequence (parseFromFileList fileList)
    -- writeFile "module.py" $ pack (show (convertToModuleList fileList verilogList))
    let moduleUnitList = convertToModuleUnitList fileList verilogList
    let moduleList = convertToModuleList verilogList
    checkNameConflict moduleUnitList
    let hierTree = convertToProjectTreeList moduleList
    let projectData = ProjectData direcTree hierTree moduleUnitList
    let topData = TopData configData projectData
    print "Already get all information."
    writeFile filepath $ toStrict $ encode topData

genTopFromFileList :: [FilePath] -> FilePath -> IO()
genTopFromFileList filepaths currentPath = do 
    let cyFiles = filter (isSuffixOf ".cy") filepaths
    if null cyFiles then
        error ("No cynide file specified.")
    else
        print ("Ready to convert:" ++ intercalate " " filepaths)
    projectPath <- getProjectPath currentPath
    projectFile <- readFile projectPath
    let projectData = decode $ fromStrict projectFile :: Maybe TopData
    let topData = getData projectData "Project"
    let moduleUnitList = getModuleList topData
    msum $ map (genTopFromFile currentPath moduleUnitList) cyFiles

genTopFromFile :: FilePath -> [ModuleUnit] -> FilePath -> IO ()
genTopFromFile currentPath mods cyName= do
    let cyFile = currentPath </> cyName
    cyData <- fromJust <$> parseCynideFile cyFile
    -- print cyData 
    -- print mods
    printVerilog (convertCynideToVerilog cyData mods) (addExtension (dropExtension cyFile) "v")

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
checkNameConflict x = msum (map (checkName x) x) where
    checkName list unit = msum (map (compareName unit) list)
    compareName (ModuleUnit na pa _ _ _) (ModuleUnit nb pb _ _ _) = 
        case and [(na == nb), (pa /= pb)] of 
            True -> print $ "Name conflict: " ++ na ++ "::" ++ pa ++ " == " ++ nb ++ "::" ++ pb
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
    executePath <- getDirPath . fromJust <$> findExecutable "ccv.exe"
    let configFilePath = executePath </> ".global.project"
    configData <- getConfigData configFilePath
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
    print $ "The email has been set to " ++ (head emails)

setAuthor :: [String] -> String -> IO ()
setAuthor [] _ = error "No author specified. ccv --author author"
setAuthor authors currentPath = do 
    executePath <- getDirPath . fromJust <$> findExecutable "ccv.exe"
    let configFilePath = executePath </> ".global.project"
    configData <- getConfigData configFilePath
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
    print $ "The author has been set to " ++ (head authors)

setEmailConfig (ConfigData au _ v1 v2) s = ConfigData au s v1 v2
setEmailProject (TopData cfd pj) s = TopData (setEmailConfig cfd s) pj
setAuthorConfig (ConfigData _ s v1 v2) au = ConfigData au s v1 v2
setAuthorProject (TopData cfd pj) s = TopData (setAuthorConfig cfd s) pj

getConfigData :: FilePath -> IO (Maybe ConfigData)
getConfigData configFilePath = do 
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
    print $ "find project root in " ++ projectFilePath
    createProjectFileWithoutConfigData projectFilePath