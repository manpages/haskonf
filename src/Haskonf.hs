module Haskconf ( run, build ) where

import           System.Directory     (getAppUserDataDirectory)
import           System.FilePath      ((</>))
import           System.Info          (arch, os)
import           System.Posix.Process (createSession, executeFile, forkProcess,
                                       getAnyProcessStatus)
import           System.Posix.Signals (installHandler)

build :: String -> IO Bool
build pname = buildDo pname Nothing False

buildForce :: String -> IO Bool
buildForce = rebuild

rebuild :: String -> IO Bool
rebuild pname = buildDo pname Nothing True

buildDo :: String -> Maybe [String] -> Bool -> IO Bool
buildDo pname Nothing   force = (flip . buildDo) pname force defaultFlags $ pname ++ "-" ++ arch ++ "-" os
buildDo pname (Just fs) force = do
  dir <- getAppUserDataDirectory pname
  let binn = getBinName pname
      binf = dir </> binn
      base = dir </> pname
      err  = base ++ ".errors"
      src  = base ++ ".hs"
      lib  = dir </> "lib"
  libTs <- mapM getModTime . Prelude.filter isSource =<< allFiles lib
  srcT <- getModTime src
  binT <- getModTime binf
  if force || any (binT <) (src : libTs)
    then do
      uninstallSignalHandlers
      status <- bracket (openFile err WriteMode) hClose $ \h -> waitForProcess =<< runProcess "ghc" fs (Just dir)
                                                                Nothing Nothing Nothing (Just h)
      installSignalHandlers
      return (status == ExitSuccess)
    else
      return True

getBinName :: String -> String
getBinName pname = pname ++ "-" ++ arch ++ "-" ++ os

defaultFlags :: String -> [String]
defaultFlags o =  ["--make", pname ++ ".hs", "-i", "-ilib", "-fforce-recomp",
                   "-main-is", "main", "-v0", "-o", o]

installSignalHandlers :: IO ()
installSignalHandlers = do
    installHandler openEndedPipe Ignore Nothing
    installHandler sigCHLD Ignore Nothing
    (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

uninstallSignalHandlers :: IO ()
uninstallSignalHandlers = do
    installHandler openEndedPipe Default Nothing
    installHandler sigCHLD Default Nothing
    return ()
