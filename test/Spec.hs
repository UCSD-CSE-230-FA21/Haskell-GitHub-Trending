module Main where

import Data (dataTest)
import Storage (storageTest)
import qualified Model.Lib as LB (version)

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Split

versionTest :: TestTree
versionTest = testCase "Testing version number" $ 
    assertEqual "Version number should be a three dot number" 3 $ length $ splitOn "." LB.version

libTest :: TestTree
libTest = testGroup "Lib Test" [versionTest]

-- Entry Point

tests :: TestTree
tests = testGroup "Client Tests" [dataTest, storageTest, libTest]

main :: IO ()
main = defaultMain tests
