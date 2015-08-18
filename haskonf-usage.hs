module Main (main) where

import           System.Environment (getArgs)

main :: IO ()
main = do 
  args <- getArgs
  putStrLn $ show args
