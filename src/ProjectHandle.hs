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

data Direction = Input | Output | InOut deriving (Show, Typeable, Data, Generic)

instance NFData Direction

data ElemType  = Wire | Register deriving (Show, Typeable, Data, Generic)

instance NFData ElemType

data Version = Version {
    v0 :: Int,
    v1 :: Int,
    v2 :: Int,
    v3 :: Int
} deriving (Show, Typeable, Data, Generic)

instance NFData Version

data DirecTree = DirecTree {
    node_name :: String,
    tree_node :: [DirecTree]
} deriving (Show, Typeable, Data, Generic)

instance NFData DirecTree

data ProjectTree = ProjectTree {
    hier_name   :: String,
    mod         :: String,
    file        :: String,
    hier_node   :: [ProjectTree]
} deriving (Show, Typeable, Data, Generic)

instance NFData ProjectTree

data ModuleUnit = ModuleUnit {
    mod_name    :: String,
    mod_note    :: String,
    paras       :: [Parameter],
    ports       :: [Port]
} deriving (Show, Typeable, Data, Generic)

instance NFData ModuleUnit

data Parameter = Parameter {
    para    :: String,
    value   :: String
} deriving (Show, Typeable, Data, Generic)

instance NFData Parameter

data Port = Port {
    port        :: String,
    direction   :: Direction,
    eletype     :: ElemType,
    port_len    :: String,
    port_wid    :: String,
    port_note   :: String
} deriving (Show, Typeable, Data, Generic)

instance NFData Port

data ConfigData = ConfigData {
    author      :: String,
    contact     :: String,
    version     :: Version,
    verstep     :: Version
} deriving (Show, Typeable, Data, Generic)

instance NFData ConfigData

data ProjectData = ProjectData {
    directory   :: DirecTree,
    hierarchy   :: ProjectTree,
    modules     :: [ModuleUnit]
} deriving (Show, Typeable, Data, Generic)

instance NFData ProjectData

data TopData = TopData ConfigData ProjectData deriving (Show, Typeable, Data, Generic)

instance NFData TopData

-- This splice will derive instances of ToJSON and FromJSON for us.

$(deriveJSON defaultOptions ''ConfigData)
$(deriveJSON defaultOptions ''ProjectData)
$(deriveJSON defaultOptions ''TopData)
$(deriveJSON defaultOptions ''Port)
$(deriveJSON defaultOptions ''Parameter)
$(deriveJSON defaultOptions ''ModuleUnit)
$(deriveJSON defaultOptions ''ProjectTree)
$(deriveJSON defaultOptions ''DirecTree)
$(deriveJSON defaultOptions ''Version)
$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''ElemType)

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
            createProjectFile projectPath globalConfigJson
        else do
            let configData = fromJust maybeConfigData
            print configData
            print "Global configuration file is found."
            createProjectFile projectPath configData
    else do
        writeFile configFilePath $ encode globalConfigJson
        print "Reinitialized Configuration File."
        createProjectFile projectPath globalConfigJson

createProjectFile :: String -> ConfigData -> IO()
createProjectFile path configData = do
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
    -- let moduleList = 
    -- let hierTree = 
    

toDirecTree :: DirTree String -> DirecTree
toDirecTree (File name file) = DirecTree name []
toDirecTree (Dir  name dir)  = DirecTree name $ map toDirecTree dir

getDirFileName :: [String] -> [String] -> DirTree String -> [String]
getDirFileName path fileList (File name _)   = (intercalate "\\" (path ++ [name])) : fileList
getDirFileName path fileList (Dir  name dir) = concat $ map (getDirFileName (path ++ [name]) fileList) dir

getFileList :: [String] -> DirTree String -> [String]
getFileList path (Dir _ dir) = filter (multSuffix allowedFileExtension) $ getDirFileName path [] (Dir "." dir)

multSuffix :: [String] -> String -> Bool
multSuffix cond str = or $ map ($ str) (map isSuffixOf cond)

