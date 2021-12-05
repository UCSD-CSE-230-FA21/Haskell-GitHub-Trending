module View.State where

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

import qualified Model.Data as MD
import qualified Network as MN


import Data.Time.Clock (getCurrentTime, utctDay)

data AppState = AppState {
    trending::(L.List () MD.Repository),
    readme::MD.Readme,
    query::MD.TrendingQuery
}


getAppState :: MD.TrendingQuery -> Maybe MD.RepositoryIdentifier -> IO (AppState)
getAppState q (Just id) =
    do
        MD.TrendingResponse i rs <-  MN.getTrendingRequest q
        rm <-  MN.getReadmeRequest id
        return $  AppState  (L.list () (Vec.fromList rs) 1) rm q 
getAppState q Nothing =
    do
        MD.TrendingResponse i rs <-  MN.getTrendingRequest q
        return $  AppState  (L.list () (Vec.fromList rs) 1) (MD.Readme "empty"  "empty")  q 