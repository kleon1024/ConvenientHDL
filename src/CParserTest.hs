--------------------------------------------------------------------------------

module Language.Cynide.ParserTest where

import Prelude hiding (catch)

import System.Directory             (getDirectoryContents)
import System.FilePath              ((</>), replaceExtension, takeExtension)
import Text.PrettyPrint             (render)
import Text.Parsec                  (parse)
import Text.Parsec.ByteString       (parseFromFile)
import System.IO

import Language.Cynide.Parser
import Language.Cynide.PrettyPrint (ppCynide)
import Language.Cynide.Syntax      (Cynide)

--------------------------------------------------------------------------------

examples_dir :: FilePath
examples_dir = ""

check_all :: IO ()
check_all
  = do files <- getDirectoryContents examples_dir
       sequence_ [ check f
                   | f <- files
                   , takeExtension f == ".ccv"
                 ]
       return ()

check :: FilePath -> IO ()
check baseName
  = do result1 <- parseFromFile cynideFile fp
       case result1 of
         Left err1  -> do writeFile fp_err1 (show err1 ++ "\n")
                          putStrLn ("Fail1: " ++ baseName)
         Right ast1 -> do let str1 = render (ppCynide ast1)
                          writeFile fp1 str1
                          result2 <- parseFromFile cynideFile fp1
                          case result2 of
                            Left err2  -> do writeFile fp_err2 (show err2 ++ "\n")
                                             putStrLn ("Fail2: " ++ baseName)
                            Right ast2 -> do let str2 = render (ppCynide ast2)
                                             writeFile fp2 str2
                                             --print $ show ast1
                                             --print $ show ast2
                                             if (ast1 == ast2)
                                                then putStrLn ("Match: " ++ baseName)
                                                else do writeFile fp_err3 ""
                                                        putStrLn ("Not match: " ++ baseName)
  where
    fp      = examples_dir </> baseName
    fp1     = replaceExtension fp "2"
    fp2     = replaceExtension fp "3"
    fp_err1 = replaceExtension fp "err1"
    fp_err2 = replaceExtension fp "err2"
    fp_err3 = replaceExtension fp "err3"

parseCynide :: String -> Cynide
parseCynide x
  = case parse cynideFile "" x of
      Left err -> error (show err)
      Right y  -> y

test :: FilePath -> IO ()
test fp
  = do x <- run fp
       putStrLn (render (ppCynide x))

run :: FilePath -> IO Cynide
run fp
  = do x <- parseFromFile cynideFile fp
       case x of
         Left err  -> error (show err)
         Right y   -> return y

hah :: FilePath -> IO ()
hah fp = do
  result1 <- parseFromFile cynideFile fp
  case result1 of
        Left err1  -> do putStrLn "Error"
                         putStrLn $ show err1
        Right ast1 -> putStrLn $ show ast1

--------------------------------------------------------------------------------

