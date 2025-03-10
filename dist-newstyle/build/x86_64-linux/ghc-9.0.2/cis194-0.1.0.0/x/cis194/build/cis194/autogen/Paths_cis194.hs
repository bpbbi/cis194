{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_cis194 (
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

bindir     = "/home/qd/.cabal/bin"
libdir     = "/home/qd/.cabal/lib/x86_64-linux-ghc-9.0.2/cis194-0.1.0.0-inplace-cis194"
dynlibdir  = "/home/qd/.cabal/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/qd/.cabal/share/x86_64-linux-ghc-9.0.2/cis194-0.1.0.0"
libexecdir = "/home/qd/.cabal/libexec/x86_64-linux-ghc-9.0.2/cis194-0.1.0.0"
sysconfdir = "/home/qd/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cis194_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cis194_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cis194_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cis194_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cis194_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cis194_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
