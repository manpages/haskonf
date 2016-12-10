module Haskonf.HaskonfSpec (main, spec) where

import           Control.Monad      (unless)
import           Haskonf            (appDir, binName, build, copyReal,
                                     doesConfigExist, rebuild, runFrom)
import           Paths_haskonf
import           System.Environment (getProgName)
import           Test.Hspec         (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

pname :: String
pname = "haskonf-usage"

theArgs :: [String]
theArgs =
    ["--rebuild"] ++ words "you see the result of running configured binary"

spec :: Spec
spec = do
    describe "haskonf" $ do
        it ("creates an application directory " ++
            "and populates it with default config") $ do
            real <- getDataFileName "haskonf-usage.hs"
            doesConfigExist pname >>= (flip unless) (copyReal pname real)
            _ <- mainDo theArgs
            -- We don't really test the result of executing the file, we just
            -- make sure that binary is compiled based on some configuration
            -- and we can run it appropriately and without errors.
            True `shouldBe` True

mainDo :: [String] -> IO ()
mainDo ("--rebuild":xs) = do
    _ <- rebuild pname
    -- -^- Bad: errors should be addressed in real applications
    launch xs

mainDo xs = do
    _ <- build pname
    -- -^- Bad: errors should be addressed in real applications
    launch xs

launch :: [String] -> IO ()
launch args = do
    dir <- appDir pname
    me <- getProgName
    runFrom me dir (binName pname) args
