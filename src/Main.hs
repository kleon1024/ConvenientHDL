module Main where

import Control.Monad

import System.Environment (getArgs)
import System.IO
import System.Directory

import ArgParseCCV
import ProjectHandle (initProject, genTopFromFileList, setEmail, setAuthor, updateProject)
--import VerilogParser

main = do
    (args, files) <- getArgs >>= argparse
    putStrLn $ "Flags: " ++ show args
    putStrLn $ "Files: " ++ show files
    currentPath <- getCurrentDirectory
    putStrLn $ "Execute: " ++ show currentPath

    if Initial `elem` args then 
        initProject files currentPath
    else if Top `elem` args then 
        genTopFromFileList files currentPath
    else if Email `elem` args then
        setEmail files currentPath
    else if Author `elem` args then
        setAuthor files currentPath
    else if Update `elem` args then
        updateProject currentPath
    else print "done"
