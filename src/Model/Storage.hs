{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.Storage where

import           Control.Monad.State
import           Data.Time(Day, defaultTimeLocale, formatTime, parseTimeOrError)
import           System.IO
import           System.Directory
import           Text.Parsec hiding (State)
import           Prelude hiding (init)
import           Text.Parsec.String
import qualified Data.Map.Strict as Map
import qualified Model.Data as D
import Data.Map (fromList)

data Record = Record {
    idt :: D.RepositoryIdentifier,
    updateTime :: Day
} deriving (Show, Eq)

type MyMap = Map.Map String Record

-- this dict is used to store marked repos
-- key: identifier, value: record
dictionary :: MyMap
dictionary = Map.empty

addOrUpdate :: (MonadState MyMap m, MonadIO m) => D.RepositoryIdentifier -> Record -> FilePath -> m()
addOrUpdate idt r fp = do
    let key = extractId idt
    dict <- get
    let newDict = Map.insert key r dict
    put newDict
    liftIO $  writeBookMark fp newDict

delete :: (MonadState MyMap m, MonadIO m) => D.RepositoryIdentifier -> FilePath -> m ()
delete idt fp = do
    let key = extractId idt
    dict <- get
    let newDict = Map.delete key dict
    put newDict
    liftIO $ writeBookMark fp newDict

exist :: (MonadState MyMap m, MonadIO m) => D.RepositoryIdentifier -> m Bool
exist idt = do
    let key = extractId idt
    dict <- get
    case Map.lookup key dict of
        Nothing -> return False
        Just _ -> return True

existMany :: (MonadState MyMap m, MonadIO m) => [D.RepositoryIdentifier] -> m [Bool]
existMany [] = do
    return []
existMany (idt:idts) = do
    mark <- exist idt
    others <- existMany idts
    return (mark : others)

pickUpDate :: (MonadState MyMap m, MonadIO m) => D.RepositoryIdentifier -> m (Maybe Day)
pickUpDate idt = do
    let key = extractId idt
    dict <- get
    case Map.lookup key dict of
        Nothing -> return Nothing
        Just r -> return (Just (updateTime r))

extractId :: D.RepositoryIdentifier -> String
extractId idt = D.ridOwner idt ++ "," ++ D.ridName idt

extractIdFromRecord :: Record -> String
extractIdFromRecord r = extractId (idt r)

generateFromId :: String -> String -> D.RepositoryIdentifier
generateFromId = D.RepositoryIdentifier

mapSize :: (MonadState MyMap m, MonadIO m) => m Int
mapSize = do
    gets Map.size



parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p = runParser p () "DUMMY"

sepParser :: Parser Char
sepParser = noneOf ",\r\n"

parseLine :: Parser Record
parseLine = do
    skipMany space
    owner <- many1 sepParser
    skipMany space
    string ","
    skipMany space
    repo <- many1 sepParser
    skipMany space
    string ","
    skipMany space
    date <- many1 sepParser

    let idt = D.RepositoryIdentifier owner repo
    let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" date
    return (Record idt time)

-- The input should be like "prefix,suffix,yyyy-mm-dd"
init :: (MonadState MyMap m, MonadIO m) => FilePath -> m()
init fp = do
    ls <- liftIO (loadBookMark fp)
    mapM_ fillIn ls

fillIn :: (MonadState MyMap m, MonadIO m) => String -> m()
fillIn line = do
    dict <- get
    case parseFromString parseLine line of
        Left err -> put dict    -- parse error
        Right r -> put (Map.insert (extractIdFromRecord r) r dict)

loadBookMark :: FilePath -> IO [String]
loadBookMark fp = do
    exist <- doesFileExist fp
    unless exist $ writeFile fp ""
    contents <- readFile fp
    let linesOfFile = lines contents
    return linesOfFile

writeBookMark :: FilePath -> MyMap -> IO ()
writeBookMark fp dict = do
    writeFile fp ""
    mapM_ writeLine (Map.toList dict)
    where
        writeLine (_, v) = do
            let idt = extractIdFromRecord v
            let date = formatTime defaultTimeLocale "%Y-%m-%d" (updateTime v)
            let line = idt ++ "," ++ date ++ "\r\n"
            appendFile fp line
