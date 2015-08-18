module Main (main) where

import           Control.Monad      (unless)
import           Haskonf            (appDir, binName, build, copyReal,
                                     doesConfigExist, rebuild, runFrom)
import           Paths_haskonf
import           System.Environment (getArgs, getProgName)

pname :: String
pname = "haskonf-usage"

main :: IO ()
main = do
  real <- getDataFileName "haskonf-usage.hs"
  doesConfigExist pname >>= (flip unless) (copyReal pname real)
  args <- getArgs
  mainDo args

mainDo :: [String] -> IO ()
mainDo ("--rebuild" : xs) = do
  _ <- rebuild pname -- Bad: errors should be addressed in real applications
  launch xs
mainDo xs = do
  _ <- build pname   -- Bad: errors should be addressed in real applications
  launch xs

launch :: [String] -> IO ()
launch args = do
  dir  <- appDir  pname
  me   <- getProgName
  runFrom me dir (binName pname) args
