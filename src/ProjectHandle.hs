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
import Data.ByteString.Lazy (readFile, writeFile)
import Data.ByteString.Lazy.Char8 (pack)

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO hiding (writeFile, readFile)
import System.Directory
import System.Directory.Tree
import System.FilePath.Windows ((</>)) 

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


initProject :: [String] -> String -> IO()
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

    fileExist <- doesFileExist configFilePath
    if fileExist then do
        configFile <- readFile configFilePath
        let maybeConfigData = decode configFile :: Maybe ConfigData

        if isNothing maybeConfigData then do
            writeFile configFilePath $ encode globalConfigJson
            print "Reinitialized Configuration File."
            createProjectFile projectFilePath projectPath globalConfigJson
        else do
            let configData = fromJust maybeConfigData
            print configData
            print "Global configuration file is found."
            createProjectFile projectFilePath projectPath configData
    else do
        writeFile configFilePath $ encode globalConfigJson
        print "Reinitialized Configuration File."
        createProjectFile projectFilePath projectPath globalConfigJson

createProjectFile :: String -> FilePath -> ConfigData -> IO()
createProjectFile filepath path configData = do
    subDirTree <- dirTree <$> readDirectory path
    -- writeFile "dirtree.txt" (pack (show subDirTree))
    if successful subDirTree then do
        print "Changing to file directory ..."
    else do
        print "Failed to read file directory."
        return ()
    let direcTree = toDirecTree subDirTree
    let fileList = getFileList [] subDirTree
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
    writeFile filepath $ encode topData

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
getFileList path (Dir _ dir) = filter (multSuffix allowedFileExtension) $ getDirFileName path [] (Dir "." dir)
getFileList _ _ = []


multSuffix :: [String] -> String -> Bool
multSuffix cond str = or $ map ($ str) (map isSuffixOf cond)