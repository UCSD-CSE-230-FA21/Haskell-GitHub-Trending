{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module View.Trending where
import Lens.Micro ((^.))
import Control.Monad (void)


import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)
import Data.Time.Clock (getCurrentTime, utctDay)

import qualified Model.Lib as ML
import qualified Network as MN
import qualified Model.Data as MD
import qualified Brick.Main as M
import Control.Monad.RWS.Lazy (MonadIO(liftIO))

import qualified View.Readme as VR
import qualified View.Filter as VF
import qualified View.State as VS
--drawUI :: (Show a) => L.List () a -> [Widget ()]
--drawUI l = [ui]
--    where
--        label = str "Github Trending " <+> cur <+> str " of " <+> total
--        cur = case l^.L.listSelectedL of
--                Nothing -> str "-"
--                Just i -> str (show (i + 1))
--        total = str $ show $ Vec.length $ l^.L.listElementsL
--        box = B.borderWithLabel label $
--              hLimit 205 $
--              vLimit 15 $
--              L.renderList listDrawElement True l
--       ui = C.vCenter $ vBox [ C.hCenter box
--                              , str " "
--                              , C.hCenter $ str "Press Enter to see ReadMe"
--                              , C.hCenter $ str "Press u to refresh"
--                              , C.hCenter $ str "Press f to set filter"
--                              , C.hCenter $ str "Press Esc to exit."
--                              ]

drawTrending :: VS.AppState -> [Widget ()]
drawTrending (VS.AppState l r q) = [ui]
    where
        label = str "Github Trending " <+> cur <+> str " of " <+> total
        cur = case l^.L.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.L.listElementsL
        box = B.borderWithLabel label $
              hLimit 205 $
              vLimit 15 $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press Enter to see ReadMe"
                              , C.hCenter $ str "Press u to refresh"
                              , C.hCenter $ str "Press f to set filter"
                              , C.hCenter $ str "Press Esc to exit."
                              ]


appEvent :: VS.AppState -> T.BrickEvent () e -> T.EventM () (T.Next VS.AppState)
appEvent s@(VS.AppState l r q) (T.VtyEvent e) =
    case e of
        -- Todo: emplement filter
        V.EvKey (V.KChar 'f') [] -> do
            st <- liftIO $ M.defaultMain theVFApp initialState
            today <- liftIO $ utctDay <$> getCurrentTime
            let
                s1 = unlines $ E.getEditContents $ st^.VF.edit1
                s2 = unlines $ E.getEditContents $ st^.VF.edit2
                s3 = unlines $ E.getEditContents $ st^.VF.edit3
                s4 = unlines $ E.getEditContents $ st^.VF.edit4
                lan = if all isSpace s1 then "*" else (VF.trim s1)
                dat = if all isSpace s2 then today else (VF.parseDay $ VF.trim s2)
                pag = if all isSpace s3 then 1 else (read s3::Int)
                per = if all isSpace s4 then 10 else (read s4::Int)
            newState <- liftIO $ VS.getAppState (MD.TrendingQuery lan dat pag per) Nothing
            M.continue newState
    -- M.suspendAndResume $ M.defaultMain VF.theApp VF.initialState
        V.EvKey V.KEsc [] -> M.halt s
        
        V.EvKey V.KEnter [] -> 
            case l^.L.listSelectedL of
                Nothing -> M.continue s
                Just i -> do
                    state <- liftIO $ VS.getAppState q (Just (MD.identifier ((l^.L.listElementsL) Vec.! i)) ) 
                    M.suspendAndResume $ M.defaultMain VR.theApp state

        ev -> M.continue =<< handleTrendingList ev s
appEvent s _ = M.continue s

handleTrendingList :: V.Event -> VS.AppState -> T.EventM () (VS.AppState)
handleTrendingList e s@(VS.AppState theList r q) = do
    nextList <- L.handleListEvent e theList
    return $ VS.AppState nextList r q


listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ selStr (show a)


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
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


theVFApp :: M.App VF.St e VF.Name
theVFApp =
    M.App { M.appDraw = VF.drawUI
          , M.appChooseCursor = VF.appCursor
          , M.appHandleEvent = VF.appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const VF.theMap
          }


initialState :: VF.St
initialState =
    VF.St (F.focusRing [VF.Edit1, VF.Edit2, VF.Edit3, VF.Edit4])
       (E.editor VF.Edit1 (Just 1) "")
       (E.editor VF.Edit2 (Just 1) "")
       (E.editor VF.Edit3 (Just 1) "")
       (E.editor VF.Edit4 (Just 1) "")

main :: IO ()
main = do
    today <- utctDay <$> getCurrentTime
    as <- VS.getAppState (MD.TrendingQuery "*" today 1 10) Nothing
    void $ M.defaultMain theApp as 
