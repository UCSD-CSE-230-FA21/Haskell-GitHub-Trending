{-# LANGUAGE OverloadedStrings #-}
module Data where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Model.Data as MD

-- Mainly this file will use Unit test since they are mostly huge object

base64DecodeTest :: TestTree
base64DecodeTest = testGroup "Testing base64Decode"
  [ testCase "Should decode without issue" $
      MD.decodeBase64 "SGVsbG8gV29ybGQhCg=="
    @?= "Hello World!\n" 
  , testCase "Multi Line" $
    MD.convertContent "SGVsbG8gV29ybGQhCg==\nSGVsbG8gV29ybGQhCg=="
    @?= "Hello World!\nHello World!\n"
  ]

parseReadmeTest :: TestTree
parseReadmeTest = testGroup "parseReadmeTest" 
  [ testCase "Should Parse without issue" $
        case MD.convertReadmeContent $ MD.Readme "SGVsbG8gV29ybGQhCg==" "base64" of
            Right msg -> msg
            Left err -> err
      @?= "Hello World!\n"
  , testCase "Unable to decode non-base64" $
      case MD.convertReadmeContent $ MD.Readme "not base64" "not supported method" of
          Right _ -> True
          Left _ -> False
      @?= False
  ]


dataTest :: TestTree
dataTest = testGroup "Data Test" [base64DecodeTest, parseReadmeTest]