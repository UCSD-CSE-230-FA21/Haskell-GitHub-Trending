{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module View.Filter where

import Lens.Micro
import Lens.Micro.TH
import Data.Time (Day)
import Data.Char (isSpace)
import Data.Time.Clock (getCurrentTime, utctDay)
import Control.Monad.RWS.Lazy (MonadIO(liftIO))
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Data.Time.Format as DTF
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

-- import qualified View.Trending as VT
import qualified View.State as VS
import qualified Model.Data as MD
import Data.Maybe ( fromMaybe )
import Text.Read (readMaybe)
data Name = Edit1
          | Edit2
          | Edit3
          | Edit4
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       , _edit2 :: E.Editor String Name
       , _edit3 :: E.Editor String Name
       , _edit4 :: E.Editor String Name
       }

makeLenses ''St


drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)
        e3 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit3)
        e4 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit4)
        ui = C.center $
            (str "Language:          " <+> (hLimit 30 $ vLimit 5 e1)) <=>
            str " " <=>
            (str "Date (yyyy-mm-dd): " <+> (hLimit 30 $ vLimit 5 e2)) <=>
            str " " <=>
            (str "Page (number):     " <+> (hLimit 30 $ vLimit 5 e3)) <=>
            str " " <=>
            (str "PerPage (number):  " <+> (hLimit 30 $ vLimit 5 e4)) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to confirm."

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] ->  M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev

        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
               Just Edit3 -> T.handleEventLensed st edit3 E.handleEditorEvent ev
               Just Edit4 -> T.handleEventLensed st edit4 E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)




----------------------------- helper functions -----------------------------

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

parseDay :: String -> Maybe Day
parseDay s = DTF.parseTimeM True DTF.defaultTimeLocale "%Y-%-m-%-d" s

----------------------------- helper functions -----------------------------

theVFApp :: M.App St e Name
theVFApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

initialState :: St
initialState =
    St (F.focusRing [Edit1, Edit2, Edit3, Edit4])
       (E.editor Edit1 (Just 1) "")
       (E.editor Edit2 (Just 1) "")
       (E.editor Edit3 (Just 1) "")
       (E.editor Edit4 (Just 1) "")

startFilter :: IO MD.TrendingQuery
startFilter = do
    st <- liftIO $ M.defaultMain theVFApp initialState
    today <- liftIO $ utctDay <$> getCurrentTime
    let
        s1 = unlines $ E.getEditContents $ st^.edit1
        s2 = unlines $ E.getEditContents $ st^.edit2
        s3 = unlines $ E.getEditContents $ st^.edit3
        s4 = unlines $ E.getEditContents $ st^.edit4
        lan = if all isSpace s1 then "*" else (trim s1)
        dat = fromMaybe today (parseDay $ trim s2)
        pag' = fromMaybe 1 (readMaybe s3)
        per' = fromMaybe 10 (readMaybe s4)
        pag = if pag'<0 || pag'>50 then 1 else pag'
        per = if per'<0 || per'>100 then 10 else per'
    return (MD.TrendingQuery lan dat pag per)