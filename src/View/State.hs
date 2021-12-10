module View.State where

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

import qualified Model.Data as MD
import qualified Network as MN
import qualified Bookmark as MB
import qualified Model.Storage as MS

import Data.Time.Clock (getCurrentTime, utctDay)
import Lens.Micro ((^.))

data AppState = AppState {
    trending::(L.List () MD.Repository),
    readme::MD.Readme,
    query::MD.TrendingQuery,
    bookmark::[Bool],
    shouldExit::Bool,
    repoIndex::Int
}


getAppState :: MD.TrendingQuery -> Int -> Maybe MD.RepositoryIdentifier -> IO (AppState)
getAppState q ind (Just id) =
    do
        MD.TrendingResponse i rs <-  MN.getTrendingRequest q
        rm <-  MN.getReadmeRequest id
        bm <- MB.batchQuery (getIds rs) MS.defaultPath 
        return $  AppState  (L.list () (Vec.fromList rs) 1) rm q bm False ind
getAppState q ind Nothing =
    do
        MD.TrendingResponse i rs <-  MN.getTrendingRequest q
        bm <- MB.batchQuery (getIds rs) MS.defaultPath 
        return $  AppState  (L.list () (Vec.fromList rs) 1) (MD.Readme "empty"  "empty") q bm False ind

getEmptyAppState :: MD.TrendingQuery  -> Bool -> IO (AppState)
getEmptyAppState q flag = return (AppState (L.list () (Vec.fromList []) 1) (MD.Readme "empty"  "empty") q [] flag 0)
        
updateBookmark :: AppState -> IO (AppState)
updateBookmark (AppState tr rm q _ _ _) = do
        bm <- MB.batchQuery (getIds (Vec.toList (tr^.L.listElementsL))) MS.defaultPath 
        return $  AppState tr rm q bm False 0

getIds :: [MD.Repository] -> [MD.RepositoryIdentifier]
getIds rs = map getId rs
    where
        getId (MD.Repository id _ _ _ _ _) = id