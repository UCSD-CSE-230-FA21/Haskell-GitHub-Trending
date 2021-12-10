{-# LANGUAGE OverloadedStrings #-}

module Filter where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (parseTimeOrError)
import Data.Time.Format (defaultTimeLocale)

import qualified View.Filter as VF

filterHelperFuncTest :: TestTree
filterHelperFuncTest = testGroup "Testing filterHelperFunc"
    [
        testCase "Should parse input correctly" $ do
            let originStr = " java \n "
            let targetStr = "java"
            let newStr = VF.trim originStr
            newStr @?= targetStr,
        testCase "Should parse valid input date correctly" $ do
            let input = "2021-11-3"
            let targetDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2021-11-03"
            case VF.parseDay input of
                Just d -> assertEqual ("Day parse error: " ++ show d ++ " expected: " ++ show targetDay) d targetDay
                Nothing -> assertFailure ("Could not parse date: " ++ input),
        testCase "Should not parse invalid input" $ do
            let input = "asdfasf"
            case VF.parseDay input of
                Just d -> assertFailure ("Should not parse invalid date: " ++ input ++ " to: " ++ show d)
                Nothing -> return ()
    ]

filterTest :: TestTree
filterTest = testGroup "Filter Test" [filterHelperFuncTest]