{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module View.Readme where
import Lens.Micro ((^.))
import Control.Monad (void)

import Brick (Widget, simpleMain, (<+>), str, txtWrap, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as Core
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import qualified Data.Text as T
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Both)
  , VScrollBarOrientation(..)
  , HScrollBarOrientation(..)
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , hBox
  , fill
  , viewport
  , padRight
  , withVScrollBars
  , withHScrollBars
  , withHScrollBarRenderer
  , withVScrollBarHandles
  , withHScrollBarHandles
  , withClickableHScrollBars
  , withClickableVScrollBars
  , ScrollbarRenderer(..)
  , scrollbarAttr
  , scrollbarHandleAttr
  )
import Web.Browser (openBrowser)
import Brick.Util (fg, on)
import Data.Time.Clock (getCurrentTime, utctDay)
-- import Data.Text (pack)

import qualified Model.Lib as ML
import qualified Network as MN
import qualified Model.Data as MD
import qualified Brick.Main as M
import Control.Monad.RWS.Lazy (MonadIO(liftIO))

import qualified View.State as VS

data Name = VP1 | VP2 | SBClick T.ClickableScrollbarElement Name
          deriving (Ord, Show, Eq)


vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

vp2Scroll :: M.ViewportScroll Name
vp2Scroll = M.viewportScroll VP2


appEvent :: VS.AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next VS.AppState)
appEvent l (T.VtyEvent (V.EvKey V.KRight []))  = M.hScrollBy vp1Scroll 1 >> M.continue l
appEvent l (T.VtyEvent (V.EvKey V.KLeft []))   = M.hScrollBy vp1Scroll (-1) >> M.continue l
appEvent l (T.VtyEvent (V.EvKey V.KDown []))   = M.vScrollBy vp2Scroll 1 >> M.continue l
appEvent l (T.VtyEvent (V.EvKey V.KUp []))     = M.vScrollBy vp2Scroll (-1) >> M.continue l
appEvent l (T.VtyEvent (V.EvKey V.KEsc []))    = M.halt l
appEvent l (T.MouseDown (SBClick el n) _ _ _) = do
    case n of
        VP1 -> do
            let vp = M.viewportScroll VP1
            case el of
                T.SBHandleBefore -> M.hScrollBy vp (-1)
                T.SBHandleAfter  -> M.hScrollBy vp 1
                T.SBTroughBefore -> M.hScrollBy vp (-10)
                T.SBTroughAfter  -> M.hScrollBy vp 10
                T.SBBar          -> return ()
        VP2 -> do
            let vp = M.viewportScroll VP2
            case el of
                T.SBHandleBefore -> M.vScrollBy vp (-1)
                T.SBHandleAfter  -> M.vScrollBy vp 1
                T.SBTroughBefore -> M.vScrollBy vp (-10)
                T.SBTroughAfter  -> M.vScrollBy vp 10
                T.SBBar          -> return ()
        _ ->
            return ()
    M.continue $ l

        
appEvent l _ = M.continue l

customScrollbars :: ScrollbarRenderer n
customScrollbars =
    ScrollbarRenderer { renderScrollbar = fill '^'
                      , renderScrollbarTrough = fill ' '
                      , renderScrollbarHandleBefore = str "<<"
                      , renderScrollbarHandleAfter = str ">>"
                      }

drawReadme :: VS.AppState -> [Widget Name]
drawReadme as@(VS.AppState l r q _) = [ui]
    where
        Just text = T.stripPrefix (T.pack "Right ") $ T.pack $ show $ MD.convertReadmeContent r
        texts = T.splitOn "\\n" $ text

        ui = C.center $ hLimit 300 $ vLimit 100 $
             (vBox [ pair
                  --  , C.hCenter (str "README.md")
                  --  , vBox $ map txtWrap texts
                   ])

        pair = 
          -- hBox [ 
                      B.border $

                      withClickableHScrollBars SBClick $
                      withHScrollBars OnBottom $
                      withHScrollBarRenderer customScrollbars $
                      withHScrollBarHandles $
                      viewport VP1 Horizontal $
                      hLimit 300 $
                      vLimit 100 $

                      withClickableVScrollBars SBClick $
                      withVScrollBars OnLeft $
                      withVScrollBarHandles $

                      viewport VP2 Both $
                      hLimit 300 $
                      vLimit 1000 $
                      vBox $ map txtWrap texts

                    -- ]


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App VS.AppState e Name
theApp =
    M.App { M.appDraw = drawReadme
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }


