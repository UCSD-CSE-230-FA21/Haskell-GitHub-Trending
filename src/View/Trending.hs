{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module View.Trending where
import Lens.Micro ((^.))
import Control.Monad (void)


import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Widgets.Border as WB
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import  Brick.Widgets.Table
import qualified Brick.Widgets.Center as WC
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , str
  , vLimit
  , hLimit
  , vBox
  , hBox
  , withAttr
  ,txt
  )
import Brick.Util (fg, on)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Text as DT
import qualified Data.Map as DM
import qualified Data.Foldable as DF

import qualified Model.Lib as ML
import qualified Network as MN
import qualified Model.Data as MD
import qualified Bookmark as B
import qualified Brick.Main as M
import Control.Monad.RWS.Lazy (MonadIO(liftIO))

import qualified View.Readme as VR
import qualified View.Filter as VF
import qualified View.State as VS
import View.Filter (parseDay)


drawTrending :: VS.AppState -> [Widget ()]
drawTrending (VS.AppState l r q bm _ _) = [ui]
    where
        label = str "Github Trending " <+> cur <+> str " of " <+> total
        cur = case l^.L.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.L.listElementsL
        box1 = WB.borderWithLabel (str "Press \"Enter\" to see ReadMe , \"s/d\" to add/delete bookmark") $
              hLimit 87 $
              vLimit 3$
              L.renderList listDrawElement True l
        box2 = WB.borderWithLabel label $
              hLimit 205 $
              vLimit 50 $
              drawTable l bm
        ui = C.hCenter $ vBox [ box1, box2, str "Press \"Esc\" to exit, \"u\" to refresh, \"f\" to set filter" ]   


appEvent :: VS.AppState -> T.BrickEvent () e -> T.EventM () (T.Next VS.AppState)
appEvent s@(VS.AppState l r q bm _ _) (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'f') [] -> do
            jumpState <- liftIO $ VS.getEmptyAppState q False
            M.halt jumpState
        V.EvKey V.KEsc [] -> do
            jumpState <- liftIO $ VS.getEmptyAppState q True
            M.halt jumpState
        
        V.EvKey V.KEnter [] -> 
            case l^.L.listSelectedL of
                Nothing -> M.continue s
                Just i -> do
                    state <- liftIO $ VS.getAppState q i (Just (MD.identifier ((l^.L.listElementsL) Vec.! i)) )
                    M.suspendAndResume $ M.defaultMain VR.theApp state

        V.EvKey (V.KChar 's')  [] -> 
            case l^.L.listSelectedL of
                Nothing -> M.continue s
                Just i -> do        
                    _ <- liftIO $ B.addBookMark (MD.identifier ((l^.L.listElementsL) Vec.! i)) B.defaultPath         
                    state <- liftIO $ VS.updateBookmark s
                    M.continue state 
        
        V.EvKey (V.KChar 'd')  [] -> 
            case l^.L.listSelectedL of
                Nothing -> M.continue s
                Just i -> do        
                    _ <- liftIO $ B.delBookMark (MD.identifier ((l^.L.listElementsL) Vec.! i)) B.defaultPath        
                    state <- liftIO $ VS.updateBookmark s
                    M.continue state 
        
        V.EvKey (V.KChar 'u') [] -> do
            today <- liftIO $ utctDay <$> getCurrentTime
            let dat = fromMaybe today (parseDay "2010-1-1")
            state <- liftIO $ VS.getAppState (MD.TrendingQuery "*" dat 1 10) 0 Nothing
            M.continue state

        ev -> M.continue =<< handleTrendingList ev s
appEvent s _ = M.continue s

handleTrendingList :: V.Event -> VS.AppState -> T.EventM () (VS.AppState)
handleTrendingList e s@(VS.AppState theList r q bm _ _) = do
    nextList <- L.handleListEvent e theList
    return $ VS.AppState nextList r q bm False 0


listDrawElement :: Bool -> MD.Repository -> Widget ()
listDrawElement sel (MD.Repository (MD.RepositoryIdentifier owner name ) des _ _ _ _ )= 
    if sel
    then case des of
        Just s -> renderTable $ surroundingBorder False $ alignCenter 1 $ table [[txt (DT.pack name)],[txt (DT.pack s) ]]
        Nothing -> renderTable $ surroundingBorder False $ alignCenter 1 $ table [[txt (DT.pack name)],[txt (DT.pack "No description") ]]
    else renderTable $ surroundingBorder False $ alignCenter 1 $ table [[txt (DT.pack name)]]


drawTable :: (L.List () MD.Repository)->[VS.StampedBookmark] -> Widget ()
drawTable l bm= renderTable $  innerTable (Vec.toList (l^.L.listElementsL)) bm 

innerTable :: [MD.Repository]-> [VS.StampedBookmark] -> Table ()
innerTable rs bm = 
    surroundingBorder False $ 
    table $  [txt "Name", txt "Owner", txt "Star", txt "Watch", txt "Language", txt "Bookmark", txt "Marked Date"] : (zipWith repositoryToTable rs bm)

repositoryToTable :: MD.Repository-> VS.StampedBookmark -> [Widget n]
repositoryToTable (MD.Repository (MD.RepositoryIdentifier owner name ) des star fork watch language) (VS.StampedBookmark b Nothing) = 
    [ txt (DT.pack name), txt(DT.pack owner), txt(DT.pack (show star)), txt (DT.pack (show watch)), txt (DT.pack language), txt (DT.pack (show b)), txt "          " ]   
repositoryToTable (MD.Repository (MD.RepositoryIdentifier owner name ) des star fork watch language) (VS.StampedBookmark b (Just dy)) = 
    [ txt (DT.pack name), txt(DT.pack owner), txt(DT.pack (show star)), txt (DT.pack (show watch)), txt (DT.pack language), txt (DT.pack (show b)), txt (DT.pack (show dy))]   


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.green)
    , (L.listSelectedAttr,    V.white `on` V.green)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App VS.AppState e ()
theApp =
    M.App { M.appDraw = drawTrending
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }


startTrending :: MD.TrendingQuery -> IO VS.AppState
startTrending q = do
    as <- VS.getAppState q 0 Nothing
    M.defaultMain theApp as
