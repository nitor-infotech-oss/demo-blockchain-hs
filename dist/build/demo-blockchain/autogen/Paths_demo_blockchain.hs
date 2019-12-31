{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_demo_blockchain (
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

bindir     = "/home/saurabh/.cabal/bin"
libdir     = "/home/saurabh/.cabal/lib/x86_64-linux-ghc-8.4.4/demo-blockchain-0.1.0.0-CNmSbJKQFkUL6p1FYEG05X-demo-blockchain"
dynlibdir  = "/home/saurabh/.cabal/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/saurabh/.cabal/share/x86_64-linux-ghc-8.4.4/demo-blockchain-0.1.0.0"
libexecdir = "/home/saurabh/.cabal/libexec/x86_64-linux-ghc-8.4.4/demo-blockchain-0.1.0.0"
sysconfdir = "/home/saurabh/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "demo_blockchain_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "demo_blockchain_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "demo_blockchain_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "demo_blockchain_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "demo_blockchain_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "demo_blockchain_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
