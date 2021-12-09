module Main where

import qualified Model.Lib as L
import qualified Model.Data as D
import qualified Network as N
import Control.Exception
import Data.Time.Clock (getCurrentTime, utctDay)
import Brick

version :: Widget ()
version = str L.version

main :: IO ()
main = simpleMain version


-- The following is for testing/playground 
-- >>> readme
--
readme :: IO ()
readme = 
  let 
    author = "diygod";
    repo = "rsshub"; -- change to something else and see error
  in do
    r <- try $ N.getReadmeRequest $ D.RepositoryIdentifier author repo :: IO (Either SomeException D.Readme)
    case r of
        Left err -> print $ "Error: " ++ show err
        Right rd -> print $ show $ D.convertReadmeContent rd

-- >>> trending
-- "TrendingResponse {totalCount = 42434232, repos = [\"apneadiving\"\t\"Google-Maps-for-Rails\"\t\"Enables easy Google map + overlays creation in Ruby apps\"\t2285\t404\t2285\t\"JavaScript\"]}"
--
trending :: IO ()
trending = 
    let 
        page = 1;
        per_page = 1; -- save some space...
        language = "Java";
    in
        do
            today <- utctDay <$> getCurrentTime
            r <- N.getTrendingRequest $ D.TrendingQuery language today page per_page
            print $ show r
