{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Interpreter (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chovi/.cabal/bin"
libdir     = "/home/chovi/.cabal/lib/x86_64-linux-ghc-8.10.2/Interpreter-0.1.0.0-inplace-Interpreter"
dynlibdir  = "/home/chovi/.cabal/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/chovi/.cabal/share/x86_64-linux-ghc-8.10.2/Interpreter-0.1.0.0"
libexecdir = "/home/chovi/.cabal/libexec/x86_64-linux-ghc-8.10.2/Interpreter-0.1.0.0"
sysconfdir = "/home/chovi/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Interpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Interpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
