module Main where

import Control.Monad

import System.Environment (getArgs)
import System.IO
import System.Directory

import ArgParseCCV
import ProjectHandle (initProject)
--import VerilogParser

main = do
    (args, files) <- getArgs >>= argparse
    putStrLn $ "Flags: " ++ show args
    putStrLn $ "Files: " ++ show files
    currentPath <- getCurrentDirectory
    putStrLn $ "Execute: " ++ show currentPath

    if Initial `elem` args 
        then initProject files currentPath
        else putStrLn $ "done"