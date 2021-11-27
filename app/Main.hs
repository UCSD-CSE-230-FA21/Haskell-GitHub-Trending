module Main where

import qualified Model.Lib as L
import qualified Model.Data as D
import qualified Network as N
import Data.Time.Clock (getCurrentTime, utctDay)
import Brick

version :: Widget ()
version = str L.version

main :: IO ()
main = simpleMain version


-- The following is for testing/playground 
-- >>> readme
-- "SGVsbG8gV29ybGQhCg=="
-- "Right \"Hello World!\\n\""
--
readme :: IO ()
readme = 
  let 
    author = "octocat";
    repo = "hello-world";
  in do
    r <- N.getReadmeRequest $ D.RepositoryIdentifier author repo
    print $ D.rContent r
    print $ show $ D.convertReadmeContent r

-- >>> trending
-- "TrendingResponse {totalCount = 39797288, repos = 
--    [Repository {identifier = RepositoryIdentifier {ridOwner = \"apneadiving\", ridName = \"Google-Maps-for-Rails\"}, 
--                  description = \"Enables easy Google map + overlays creation in Ruby apps\", star = 2285, fork = 404, watcher = 2285, rLanguage = \"JavaScript\"}]}"
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