{-# LANGUAGE OverloadedStrings #-}

module Bookmark where

import           Control.Monad.State (evalStateT)
import           Data.Time(Day, getCurrentTime, UTCTime (utctDay))

import qualified Model.Data as D
import qualified Model.Storage as S

-- This is the service that provides bookmark information, and perform update ion disk

-- Input: A list of RepositoryIdentifier (definition in Model.Data), FilePath arg (Could use storePath in Model.Storage)
-- Output: A list of Bool values, indicating whether a repo has been marked. their positions are fixed and the same as input list.
batchQuery :: [D.RepositoryIdentifier] -> FilePath -> IO [Bool]
batchQuery idts fp = evalStateT (do {S.init fp; S.existMany idts}) S.dictionary

-- Input: One single RepositoryIdentifier (definition in Model.Data), FilePath arg (Could use storePath in Model.Storage)
-- Output: One single Bool value, indicating whether the input repo has been marked
singleQuery :: D.RepositoryIdentifier -> FilePath -> IO Bool
singleQuery idt fp = evalStateT (do {S.init fp; S.exist idt}) S.dictionary

-- Input: One single RepositoryIdentifier (definition in Model.Data), FilePath arg (Could use storePath in Model.Storage)
-- Output: Maybe Day (Nothing | Just Day), indicating the date when the input repo was marked or Nothing
dateQuery :: D.RepositoryIdentifier -> FilePath -> IO (Maybe Day)
dateQuery idt fp = evalStateT (do {S.init fp; S.pickUpDate idt}) S.dictionary

-- Input: One single RepositoryIdentifier (definition in Model.Data), FilePath arg (Could use storePath in Model.Storage)
-- Output: The size of current bookmark map
addBookMark :: D.RepositoryIdentifier -> FilePath -> IO Int
addBookMark idt fp = do
    time <- getCurrentTime
    let r = S.Record idt (utctDay time)
    evalStateT (do {S.init fp; S.addOrUpdate idt r fp; S.mapSize}) S.dictionary

-- Input: One single RepositoryIdentifier (definition in Model.Data), FilePath arg (Could use storePath in Model.Storage)
-- Output: The size of current bookmark map
delBookMark :: D.RepositoryIdentifier -> FilePath -> IO Int
delBookMark idt fp = evalStateT (do {S.init fp; S.delete idt fp; S.mapSize}) S.dictionary