{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Data where

import           Data.Time (Day)
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Base64 as B (decode)
import qualified Data.ByteString.Char8 as B (pack,unpack)
import qualified Data.List.Split as S (splitOn)
import           Data.Char (isSpace)
import           GHC.Generics

data TrendingQuery = TrendingQuery {
  tLanguage:: String,
  sinceDay :: Day,
  page :: Int,
  itemPerPage :: Int
} deriving (Show, Eq, Generic)

data TrendingResponse = TrendingResponse {
  totalCount :: Int,
  repos :: [Repository]
} deriving (Show, Generic)

data RepositoryIdentifier = RepositoryIdentifier {
  ridOwner :: String,
  ridName  :: String
} deriving (Show, Eq, Generic)

data Repository = Repository {
  identifier :: RepositoryIdentifier,
  description :: String,
  star :: Int,
  fork :: Int,
  watcher :: Int,
  rLanguage :: String
} deriving (Show, Eq, Generic)

data Readme = Readme {
  rContent :: String,
  encoding :: String
} deriving (Show, Generic)

-- parser

instance FromJSON Readme where
  parseJSON = withObject "Readme" readmeParser

readmeParser :: Object -> Parser Readme
readmeParser obj = do
  rContent <- obj .: "content"
  encoding  <- obj .: "encoding"
  return $ Readme (rstrip rContent) encoding

instance FromJSON Repository where
  parseJSON = withObject "Repository" repositoryParser

repositoryParser :: Object -> Parser Repository
repositoryParser obj = do
  description <- obj .: "description"
  star <- obj .: "stargazers_count"
  fork <- obj .: "forks"
  watcher <- obj .: "watchers"
  rLanguage <- obj .: "language" -- When query set with language, it is possible to get null
  name <- obj .: "name"
  owner <- obj .: "owner"
  owner_name <- owner .: "login"
  let 
    id = RepositoryIdentifier owner_name name;
  return $ Repository id description star fork watcher rLanguage

instance FromJSON TrendingResponse where
  parseJSON = withObject "TrendingResponse" trendingResponseParser

trendingResponseParser :: Object -> Parser TrendingResponse
trendingResponseParser obj = do
  totalCount <- obj .: "total_count"
  repositories <- obj .: "items"
  return $ TrendingResponse totalCount repositories

-- helpers
-- Can you imagine that haskell don't even have a lightweight, usable rstrip?
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

decodeBase64 :: String -> String
decodeBase64 str =
  case B.decode (B.pack str) of
    Left err -> error err
    Right msg -> B.unpack msg 

convertContent :: String -> String
convertContent str = 
  let 
    chars = S.splitOn "\n" str;
    dec = map decodeBase64 chars;
  in 
    foldl (++) "" dec
  
convertReadmeContent :: Readme -> Either String String
convertReadmeContent rd = 
  let 
    encode = encoding rd;
    content = rContent rd;
  in 
    case encode of
      "base64" -> Right $ convertContent content
      otherwise -> Left $ "Unsupported Encoding: " ++ encode
