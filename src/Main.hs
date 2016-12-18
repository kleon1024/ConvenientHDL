module Main where

import Control.Monad

import System.Environment (getArgs)
import System.IO
import System.Directory

import ArgParseCCV
import ProjectHandle
--import VerilogParser

main = do
    (args, files) <- getArgs >>= argparse
    putStrLn $ "Flags: " ++ show args
    putStrLn $ "Files: " ++ show files
    currentPath <- getCurrentDirectory
    putStrLn $ "Execute: " ++ show currentPath

    if Initial `elem` args then 
        initProject files currentPath
    else if New `elem` args then
        newVerilogFiles files currentPath
    else if Top `elem` args then 
        if Increment `elem` args then
            genTopWithVerilog files currentPath $ map show args
        else 
            genTopFromFileList files currentPath $ map show args
    else if Email `elem` args then
        setEmail files currentPath
    else if Author `elem` args then
        setAuthor files currentPath
    else if Update `elem` args then
        updateProject currentPath
    else if Merge `elem` args then
        mergeMultiFiles files currentPath $ map show args
    else if Split `elem` args then
        splitMultiFiles files currentPath
    else print "done"
