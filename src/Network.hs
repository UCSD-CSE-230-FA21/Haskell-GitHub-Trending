{-# LANGUAGE OverloadedStrings #-}

module Network where

import Control.Monad.IO.Class
import Control.Exception
import Data.Text (Text, pack)
import qualified Data.ByteString as B (append)
import qualified Data.ByteString.UTF8 as B (fromString)
import qualified Data.ByteString.Base64 as B (decode)
import qualified Data.ByteString.Char8 as B (unpack)
import Data.Aeson
import Data.Aeson.Types
import Data.Time (Day, formatTime, defaultTimeLocale)
import Network.HTTP.Req


import qualified Model.Lib as L
import qualified Model.Data as D

-- Helpers
buildHeaders :: Option https'
buildHeaders = header "User-Agent" $ B.append "Haskell Github Trending Client/" $ B.fromString L.version

-- Requests

getReadmeRequest :: D.RepositoryIdentifier -> IO (D.Readme)
getReadmeRequest id = 
  let 
    owner = pack $ D.ridOwner id;
    name = pack $ D.ridName id;
  in
  runReq defaultHttpConfig $ do
    r <- 
      req
        GET -- method
        (https "api.github.com" /: "repos" /: owner /: name /: "readme" ) -- safe by construction URL
        NoReqBody -- use built-in options or add your own
        jsonResponse -- specify how to interpret response
        $ buildHeaders -- query params, headers, explicit port number, etc.  
    return (responseBody r :: D.Readme)


getTrendingRequest :: D.TrendingQuery -> IO (D.TrendingResponse)
getTrendingRequest query = 
  let 
    lang = "language:" ++ (show (D.tLanguage query));
    day = formatTime defaultTimeLocale "%Y-%m-%d" $ D.sinceDay query;
    since = "created:>=" ++ day;
    qry = lang ++ " " ++ since;
    searchQueries = (
      ("sort" =: ("stars" :: Text))
      <> ("order" =: ("desc" :: Text))
      <> ("per_page") =: (show (D.itemPerPage query))
      <> ("page") =: (show (D.page query))
      <> ("q" =: qry));
  in
  runReq defaultHttpConfig $ do
    r <-
      req
        GET -- method
        (https "api.github.com" /: "search" /: "repositories") -- safe by construction URL
        NoReqBody -- use built-in options or add your own
        jsonResponse -- specify how to interpret response
        $ buildHeaders <> searchQueries -- query params, headers, explicit port number, etc.
    return (responseBody r :: D.TrendingResponse)
    