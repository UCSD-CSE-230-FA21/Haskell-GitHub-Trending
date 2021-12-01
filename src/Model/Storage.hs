{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Storage where

import           Control.Monad.State
import           Data.Time(Day, defaultTimeLocale, formatTime, parseTimeOrError)
import           System.IO
import           System.Directory
import           Text.Parsec hiding (State)
import           Text.Parsec.String
import qualified Data.Map.Strict as Map
import qualified Model.Data as D

data Record = Record {
    idt :: D.RepositoryIdentifier,
    updateTime :: Day
} deriving (Show, Eq)


-- this dict is used to store marked repos
-- key: identifier, value: record
dictionary :: Map.Map String Record
dictionary = Map.empty

addOrUpdate :: D.RepositoryIdentifier -> Record -> FilePath -> State (Map.Map String Record) ()
addOrUpdate idt r fp = do
    let key = extractId idt
    dict <- get
    let newDict = Map.insert key r dict
    let _ =  writeBookMark fp newDict
    put newDict

delete :: D.RepositoryIdentifier -> FilePath -> State (Map.Map String Record) ()
delete idt fp = do
    let key = extractId idt
    dict <- get
    let newDict = Map.delete key dict
    let _ = writeBookMark fp newDict
    put newDict

exist :: D.RepositoryIdentifier -> State (Map.Map String Record) Bool
exist idt = do
    let key = extractId idt
    dict <- get
    case Map.lookup key dict of
        Nothing -> return False
        Just _ -> return True

extractId :: D.RepositoryIdentifier -> String
extractId idt = D.ridOwner idt ++ "," ++ D.ridName idt

extractIdFromRecord :: Record -> String
extractIdFromRecord r = extractId (idt r)

generateFromId :: String -> String -> D.RepositoryIdentifier
generateFromId = D.RepositoryIdentifier



defaultPath :: String
defaultPath = "storage/store"

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
init :: FilePath -> StateT (Map.Map String Record) IO ()
init fp = do
    ls <- lift (loadBookMark fp)
    mapM_ fillIn ls
    lift (putStrLn "init finished")

fillIn :: String -> StateT (Map.Map String Record) IO ()
fillIn line = do
    dict <- get
    case parseFromString parseLine line of
        Left err -> put dict    -- parse error
        Right r -> put (Map.insert "s" r dict)

loadBookMark :: FilePath -> IO [String]
loadBookMark fp = do
    exist <- doesFileExist fp
    unless exist $ writeFile fp ""
    contents <- readFile fp
    let linesOfFile = lines contents
    return linesOfFile

writeBookMark :: FilePath -> Map.Map String Record -> IO ()
writeBookMark fp dict = do
    writeFile fp ""
    mapM_ writeLine (Map.toList dict)
    where
        writeLine (_, v) = do
            let idt = extractIdFromRecord v
            let date = formatTime defaultTimeLocale "%Y-%m-%d" (updateTime v)
            let line = idt ++ "," ++ line ++ "\r\n"
            appendFile fp line
