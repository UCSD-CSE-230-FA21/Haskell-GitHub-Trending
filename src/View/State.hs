module View.State where

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

import qualified Model.Data as MD
import qualified Network as MN
import qualified Bookmark as MB

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time(Day)
import Lens.Micro ((^.))


data StampedBookmark = StampedBookmark {
    isSaved::Bool,
    stamp::(Maybe Day)

}

data AppState = AppState {
    trending::(L.List () MD.Repository),
    readme::MD.Readme,
    query::MD.TrendingQuery,
    bookmark::[StampedBookmark],
    shouldExit::Bool
}


getAppState :: MD.TrendingQuery -> Maybe MD.RepositoryIdentifier -> IO (AppState)
getAppState q (Just id) =
    do
        MD.TrendingResponse i rs <-  MN.getTrendingRequest q
        rm <-  MN.getReadmeRequest id
        bm <- MB.batchQuery (getIds rs) MB.defaultPath 
        bt <- MB.batchDateQuery (getIds rs) MB.defaultPath 
        return $  AppState  (L.list () (Vec.fromList rs) 1) rm q (zipWith StampedBookmark bm bt) False
getAppState q Nothing =
    do
        MD.TrendingResponse i rs <-  MN.getTrendingRequest q
        bm <- MB.batchQuery (getIds rs) MB.defaultPath 
        bt <- MB.batchDateQuery (getIds rs) MB.defaultPath 
        return $  AppState  (L.list () (Vec.fromList rs) 1) (MD.Readme "empty"  "empty") q (zipWith StampedBookmark bm bt)  False

getEmptyAppState :: MD.TrendingQuery  -> Bool -> IO (AppState)
getEmptyAppState q flag = return (AppState (L.list () (Vec.fromList []) 1) (MD.Readme "empty"  "empty") q [] flag)
        
updateBookmark :: AppState -> IO (AppState)
updateBookmark (AppState tr rm q _ _) = do
        bm <- MB.batchQuery (getIds (Vec.toList (tr^.L.listElementsL))) MB.defaultPath
        bt <- MB.batchDateQuery (getIds (Vec.toList (tr^.L.listElementsL))) MB.defaultPath  
        return $  AppState tr rm q (zipWith StampedBookmark bm bt) False

getIds :: [MD.Repository] -> [MD.RepositoryIdentifier]
getIds rs = map getId rs
    where
        getId (MD.Repository id _ _ _ _ _) = id