module Paths_ACState (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,1,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/indika/dev/functional/explore/libraries/AcState/.cabal-sandbox/bin"
libdir     = "/Users/indika/dev/functional/explore/libraries/AcState/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/ACState-1.1.1"
datadir    = "/Users/indika/dev/functional/explore/libraries/AcState/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/ACState-1.1.1"
libexecdir = "/Users/indika/dev/functional/explore/libraries/AcState/.cabal-sandbox/libexec"
sysconfdir = "/Users/indika/dev/functional/explore/libraries/AcState/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ACState_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ACState_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ACState_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ACState_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ACState_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
