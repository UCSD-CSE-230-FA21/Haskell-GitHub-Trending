{-# LANGUAGE OverloadedStrings #-}

module Storage where

import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad.State

import qualified Model.Data as MD
import Data.Time (parseTimeOrError)
import Data.Time.Format (defaultTimeLocale)
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

mapTest :: TestTree
mapTest = testGroup "Testing map operations"
    [
        testCase "Should not find from empty map" $
            evalState (MS.exist idt1) MS.dictionary @?= False,
        testCase "Should successfully insert into map" $
            evalState (do {MS.addOrUpdate' idt1 r1; MS.addOrUpdate' idt2 r2; MS.mapSize}) MS.dictionary @?= 2,
        testCase "Should successfully delete from map" $
            evalState (do {MS.addOrUpdate' idt1 r1; MS.delete' idt1; MS.delete' idt2; MS.mapSize}) MS.dictionary @?= 0,
        testCase "Should successfully find from map" $
            evalState (do {MS.addOrUpdate' idt1 r1; MS.addOrUpdate' idt2 r2; MS.exist idt1}) MS.dictionary @?= True,
        testCase "Should test many repos marked" $
            evalState (do {MS.addOrUpdate' idt1 r1; MS.existMany [idt1, idt2]}) MS.dictionary @?= [True, False]
    ]


storageTest :: TestTree
storageTest = testGroup "Storage Test" [storageFileParsingTest, mapTest]