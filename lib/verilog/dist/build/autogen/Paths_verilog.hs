{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_verilog (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,2] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Kleon\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Kleon\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\verilog-0.2-IWu1Ys8oajMAZogbHoa0VO"
datadir    = "C:\\Users\\Kleon\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\verilog-0.2"
libexecdir = "C:\\Users\\Kleon\\AppData\\Roaming\\cabal\\verilog-0.2-IWu1Ys8oajMAZogbHoa0VO"
sysconfdir = "C:\\Users\\Kleon\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "verilog_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "verilog_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "verilog_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "verilog_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "verilog_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
