{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Control.Monad (void)



import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
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
import qualified Brick.Main as M
import Control.Monad.RWS.Lazy (MonadIO(liftIO))

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.L.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.L.listElementsL
        box1 = B.borderWithLabel label $
              hLimit 205 $
              vLimit 15 $
              L.renderList listDrawElement True l
        box2 = .borderWithLabel label $
              hLimit 205 $
              vLimit 15 $
              L.renderList listDrawElement True l
        ui = borderWithLabel (str "Hello!") $
            (center (str "Left") <+> vBorder <+> center (str "Right"))
        --ui = C.vCenter $ vBox [ C.hCenter box
        --                      , str " "
        --                      , C.hCenter $ str "Press +/- to add/remove list elements."
        --                      , C.hCenter $ str "Press Esc to exit."
        --                      ]

appEvent :: L.List () MD.Repository -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () MD.Repository))
appEvent l (T.VtyEvent e) =
    case e of
        --V.EvKey (V.KChar '-') [] ->
        --    case l^.L.listSelectedL of
        --        Nothing -> M.continue l
        --        Just i -> M.continue $ L.listRemove i l

        V.EvKey (V.KChar 'f') [] -> do 
            nl <- liftIO $ initialState "C++"
            M.continue nl

        V.EvKey V.KEsc [] -> M.halt l

        V.EvKey (V.KChar 'i') [] ->  M.suspendAndResume $ M.defaultMain theApp l

        ev -> M.continue =<< L.handleListEvent ev l
appEvent l _ = M.continue l


listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ selStr (show a)


initialState :: String -> IO (L.List () MD.Repository)
initialState l = 
    let 
      page = 1;
      per_page = 5; -- save some space...
    in
        do
            today <- utctDay <$> getCurrentTime
            MD.TrendingResponse i rs <- MN.getTrendingRequest $ MD.TrendingQuery l today page per_page
            return $ L.list () (Vec.fromList rs) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List () MD.Repository) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do 
    is <- initialState "Java"
    void $ M.defaultMain theApp is
