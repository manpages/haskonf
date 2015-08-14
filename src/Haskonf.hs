module Haskonf ( build, buildForce, rebuild, appDir, binName, runFrom ) where

import           Control.Exception.Extensible (SomeException (..), bracket, try)
import qualified Control.Exception.Extensible as E
import           Control.Monad                (filterM, when)
import           Control.Monad.Fix            (fix)
import           Data.List                    ((\\))
import           Data.Maybe                   (isJust)
import           System.Directory             (doesDirectoryExist,
                                               getAppUserDataDirectory,
                                               getDirectoryContents,
                                               getModificationTime)
import           System.Exit                  (ExitCode (..))
import           System.FilePath              (takeExtension, (</>))
import           System.Info                  (arch, os)
import           System.IO                    (IOMode (..), hClose, openFile)
import           System.Posix.Process         (executeFile, getAnyProcessStatus)
import           System.Posix.Signals         (Handler (..), installHandler,
                                               openEndedPipe, sigCHLD)
import           System.Process               (runProcess, waitForProcess)

runFrom :: String -> FilePath -> String -> [String] -> IO ()
runFrom x _ z _
  | x == z = pure ()
runFrom _ y z a = executeFile (y </> z) False a Nothing

appDir :: String -> IO FilePath
appDir = getAppUserDataDirectory

build :: String -> IO Bool
build pname = buildDo pname Nothing False

buildForce :: String -> IO Bool
buildForce = rebuild

rebuild :: String -> IO Bool
rebuild pname = buildDo pname Nothing True

buildDo :: String -> Maybe [String] -> Bool -> IO Bool
buildDo pname Nothing   force = (flip . buildDo) pname force $ Just $ defaultFlags pname
buildDo pname (Just fs) force = do
  dir <- getAppUserDataDirectory pname
  let binn = binName pname
      binf = dir </> binn
      base = dir </> pname
      err  = base ++ ".errors"
      src  = base ++ ".hs"
      lib  = dir </> "lib"
  libTs <- mapM getModTime . Prelude.filter isSource =<< allFiles lib
  srcT <- getModTime src
  binT <- getModTime binf
  if force || any (binT <) (srcT : libTs)
    then do
      uninstallSignalHandlers
      status <- bracket (openFile err WriteMode) hClose $ \h -> waitForProcess =<< runProcess "ghc" fs (Just dir)
                                                                Nothing Nothing Nothing (Just h)
      installSignalHandlers
      return (status == ExitSuccess)
    else
      return True
  where
    getModTime f = E.catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)
    isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
    allFiles t = do
      let prep = map (t</>) . Prelude.filter (`notElem` [".",".."])
      cs <- prep <$> E.catch (getDirectoryContents t) (\(SomeException _) -> return [])
      ds <- filterM doesDirectoryExist cs
      concat . ((cs \\ ds):) <$> mapM allFiles ds

binName :: String -> String
binName pname = pname ++ "-" ++ arch ++ "-" ++ os

defaultFlags :: String -> [String]
defaultFlags pname =  ["--make", pname ++ ".hs", "-i", "-ilib", "-fforce-recomp",
                       "-main-is", "main", "-v0", "-o", binName pname]

installSignalHandlers :: IO ()
installSignalHandlers = do
    _ <- installHandler openEndedPipe Ignore Nothing
    _ <- installHandler sigCHLD Ignore Nothing
    _ <- (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

uninstallSignalHandlers :: IO ()
uninstallSignalHandlers = do
    _ <- installHandler openEndedPipe Default Nothing
    _ <- installHandler sigCHLD Default Nothing
    return ()
