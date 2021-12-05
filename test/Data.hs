{-# LANGUAGE OverloadedStrings #-}
module Data where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Model.Data as MD

-- Mainly this file will use Unit test since they are mostly huge object

base64DecodeTest :: TestTree
base64DecodeTest = testGroup "Testing base64Decode"
  [ testCase "Should decode without issue" $
      case MD.decodeBase64 "SGVsbG8gV29ybGQhCg==" of
          Right msg -> msg
          Left err -> err
    @?= "Hello World!\n" 
  , testCase "Unable to decode" $ 
    case MD.decodeBase64 "something not base64" of 
        Right _ -> True
        Left _ -> False
    @?= False
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