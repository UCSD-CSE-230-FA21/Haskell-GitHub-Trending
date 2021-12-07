{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module View.Readme where
import Lens.Micro ((^.))
import Control.Monad (void)

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

import qualified Graphics.Vty as V

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
import Control.Monad.RWS.Lazy (MonadIO(liftIO))

import qualified View.State as VS

-- Todo: Show ReadMe

ui :: Widget ()
ui =
    withBorderStyle unicode $
    borderWithLabel (str "Hello!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))

appEvent :: VS.AppState -> T.BrickEvent () e -> T.EventM () (T.Next VS.AppState)
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt l
appEvent l _ = M.continue l


drawReadme :: VS.AppState -> [Widget ()]
drawReadme (VS.AppState l r q) = [ui]
    where
        text = str (show $ MD.convertReadmeContent r)
        text2 = str (show r)
        ui = C.vCenter $ vBox [C.hCenter text2]


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
    M.App { M.appDraw = drawReadme
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }


main :: IO ()
main = do
    today <- utctDay <$> getCurrentTime
    as <- VS.getAppState (MD.TrendingQuery "*" today 1 10)  Nothing
    void $ M.defaultMain theApp as 

