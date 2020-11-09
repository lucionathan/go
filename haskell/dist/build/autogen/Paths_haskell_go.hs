{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_go (
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
version = Version [0,1,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/felipe/.cabal/bin"
libdir     = "/home/felipe/.cabal/lib/x86_64-linux-ghc-8.0.2/haskell-go-0.1.1.0-37T8BUG6oTKDtktbUo1HHD"
dynlibdir  = "/home/felipe/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/felipe/.cabal/share/x86_64-linux-ghc-8.0.2/haskell-go-0.1.1.0"
libexecdir = "/home/felipe/.cabal/libexec"
sysconfdir = "/home/felipe/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_go_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_go_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_go_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_go_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_go_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_go_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
