{-# LANGUAGE OverloadedStrings #-}

module Bookmark where

import           Control.Monad.State (evalStateT)
import           Data.Time(Day, getCurrentTime, UTCTime (utctDay))

import qualified Model.Data as D
import qualified Model.Storage as S

batchQuery :: [D.RepositoryIdentifier] -> FilePath -> IO [Bool]
batchQuery idts fp = evalStateT (do {S.init fp; S.existMany idts}) S.dictionary

singleQuery :: D.RepositoryIdentifier -> FilePath -> IO Bool
singleQuery idt fp = evalStateT (do {S.init fp; S.exist idt}) S.dictionary

dateQuery :: D.RepositoryIdentifier -> FilePath -> IO (Maybe Day)
dateQuery idt fp = evalStateT (do {S.init fp; S.pickUpDate idt}) S.dictionary

addBookMark :: D.RepositoryIdentifier -> FilePath -> IO Int
addBookMark idt fp = do
    time <- getCurrentTime
    let r = S.Record idt (utctDay time)
    evalStateT (do {S.init fp; S.addOrUpdate idt r fp; S.mapSize}) S.dictionary

delBookMark :: D.RepositoryIdentifier -> FilePath -> IO Int
delBookMark idt fp = evalStateT (do {S.init fp; S.delete idt fp; S.mapSize}) S.dictionary