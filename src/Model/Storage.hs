{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Storage where

import           Control.Monad.State
import           Data.Time(Day)
import qualified Data.Map.Strict as Map

import qualified Model.Data as D

data Record = Record {
    id :: D.RepositoryIdentifier,
    updateTime :: Day
} deriving (Show, Eq)


-- this dict is used to store marked repos
-- key: identifier, value: record
dictionary :: Map.Map String Record
dictionary = Map.empty

addOrUpdate :: String -> Record -> State (Map.Map String Record) ()
addOrUpdate id r = do
    dict <- get
    put (Map.insert id r dict)

delete :: String -> State (Map.Map String Record) ()
delete id = do
    dict <- get
    put (Map.delete id dict)

extractId :: D.RepositoryIdentifier -> String
extractId id = D.ridOwner id ++ " " ++ D.ridName id

generateFromId :: String -> String -> D.RepositoryIdentifier
generateFromId = D.RepositoryIdentifier

