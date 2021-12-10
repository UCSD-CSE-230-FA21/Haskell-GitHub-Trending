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
-- import Data.Text (pack)

import qualified Model.Lib as ML
import qualified Network as MN
import qualified Model.Data as MD
import qualified Brick.Main as M
import Control.Monad.RWS.Lazy (MonadIO(liftIO))

import qualified View.State as VS


appEvent :: VS.AppState -> T.BrickEvent () e -> T.EventM () (T.Next VS.AppState)
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> M.halt l
        _ -> M.continue l
appEvent l _ = M.continue l


drawReadme :: VS.AppState -> [Widget ()]
drawReadme (VS.AppState l r q _ _) = [ui]
    where
        label = str "README.md"
        Just text = T.stripPrefix (T.pack "Right ") $ T.pack $ show $ MD.convertReadmeContent r
        texts = T.splitOn "\\n" $ text
        box = B.borderWithLabel label $
              hLimit 205 $
              vBox $ map txtWrap texts
        ui = withBorderStyle unicode $ C.vCenter box


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


