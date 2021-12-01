{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Storage where

import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad.State

import qualified Model.Data as MD
import Data.Time (parseTimeOrError)
import Data.Time.Format (defaultTimeLocale)
import System.Directory
import qualified Model.Storage as MS

idt1 = MD.RepositoryIdentifier "a1" "b-1"
idt2 = MD.RepositoryIdentifier "a-2" "b2"
day = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "1996-11-08"
r1 = MS.Record idt1 day
r2 = MS.Record idt2 day

line1 = "a1,b-1,1996-11-08"
line2 = "a-2,b2,1996-11-08"
storePath = "storage/test"

storageFileParsingTest :: TestTree
storageFileParsingTest = testGroup "Testing storageFileParsing"
    [
        testCase "Should read and parse correctly for line 1" $
            case MS.parseFromString MS.parseLine line1 of
                Left err -> False
                Right r -> r == r1
        @?= True,
        testCase "Should read and parse correctly for line 2" $
            case MS.parseFromString MS.parseLine line2 of
                Left err -> False
                Right r -> r == r2
        @?= True
    ]


fileTest :: TestTree
fileTest = testGroup "Testing IO operations"
    [
        testCase "Should not find from empty map" $ do
            f <- evalStateT (MS.exist idt1) MS.dictionary
            f @?= False,
        testCase "Should successfully insert into map" $ do
            let storePath1 = storePath ++ "1"
            s <- evalStateT (do {MS.addOrUpdate idt1 r1 storePath1; MS.addOrUpdate idt2 r2 storePath1; MS.mapSize}) MS.dictionary
            removeFile storePath1
            s @?= 2,
        testCase "Should successfully delete from map" $ do
            let storePath2 = storePath ++ "2"
            s <- evalStateT (do {MS.addOrUpdate idt1 r1 storePath2; MS.delete idt1 storePath2; MS.delete idt2 storePath2; MS.mapSize}) MS.dictionary
            removeFile storePath2
            s @?= 0,
        testCase "Should successfully find from map" $ do
            let storePath3 = storePath ++ "3"
            f <- evalStateT (do {MS.addOrUpdate idt1 r1 storePath3; MS.addOrUpdate idt2 r2 storePath3; MS.exist idt1}) MS.dictionary
            removeFile storePath3
            f @?= True,
        testCase "Should test many repos marked" $ do
            let storePath4 = storePath ++ "4"
            f <- evalStateT (do {MS.addOrUpdate idt1 r1 storePath4; MS.existMany [idt1, idt2]}) MS.dictionary
            removeFile storePath4
            f @?= [True, False],
        testCase "Should init from empty directory" $ do
            let storePath5 = storePath ++ "5"
            f <- evalStateT (do {MS.init storePath5; MS.mapSize}) MS.dictionary
            removeFile storePath5
            f @?= 0,
        testCase "Should successfully load from existing bookmark file" $ do
            f1 <- evalStateT (do {MS.init storePath; MS.mapSize}) MS.dictionary
            unless (f1==2) $ assertFailure "Failed to load one or some tuples in file"
            f2 <- evalStateT (do {MS.init storePath; MS.exist idt1}) MS.dictionary
            unless f2 $ assertFailure ("line1" ++ line1 ++ "is not in filter result")
            f3 <- evalStateT (do {MS.init storePath; MS.exist idt2}) MS.dictionary
            unless f2 $ assertFailure ("line2" ++ line2 ++ "is not in filter result"),
        testCase "Should successfully query marked date" $ do
            f1 <- evalStateT (do {MS.init storePath; MS.pickUpDate idt1}) MS.dictionary
            case f1 of
                Nothing -> assertFailure ("Could not find out specific date of idt1: " ++ show idt1)
                Just d -> assertEqual ("Wrong date picked up: " ++ show d ++ " expected: " ++ show day) d day,
        testCase "Should return Nothing when query non-existing repo" $ do
            let idt3 = MD.RepositoryIdentifier "a3" "b3"
            f1 <- evalStateT (do {MS.init storePath; MS.pickUpDate idt3}) MS.dictionary
            case f1 of
                Nothing -> assertEqual "DUMMY" f1 f1
                Just _ -> assertFailure ("Should not find out specific date of idt3: " ++ show idt3)
    ]


storageTest :: TestTree
storageTest = testGroup "Storage Test" [storageFileParsingTest, fileTest]