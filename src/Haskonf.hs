module Haskconf ( run, build ) where

import           System.Directory (getAppUserDataDirectory)
import           System.FilePath  ((</>))
import           System.Info      (arch, os)

run :: String
run = "???"

build :: String -> IO Bool
build pname = buildDo pname Nothing

buildDo :: String -> Maybe [String] -> IO Bool
buildDo pname Nothing = buildDo pname defaultFlags $ pname ++ "-" ++ arch ++ "-" os
buildDo pname (Just fs) = do
  dir <- getAppUserDataDirectory pname
  let binn = getBinName pname
      binf = dir </> binn
      base = dir </> pname
      err  = base ++ ".errors"
      src  = base ++ ".hs"
      lib  = dir </> "lib"
  return True

getBinName :: String -> String
getBinName pname = pname ++ "-" ++ arch ++ "-" ++ os

defaultFlags :: String -> [String]
defaultFlags o =  ["--make", pname ++ ".hs", "-i", "-ilib", "-fforce-recomp",
                   "-main-is", "main", "-v0", "-o", o]
