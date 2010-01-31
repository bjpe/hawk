{-# LANGUAGE TemplateHaskell #-}
module App.View.GuestbookView (showXhtml) where

import App.Model.GuestbookEntry

import Hawk.Controller
import Hawk.View.TemplateView
import Hawk.View.Template.DataType
import qualified Hawk.View.Template.HtmlHelper as H

import Data.Time
import System.Locale

formatTi :: UTCTime -> [XmlTree]
formatTi t = [H.text (formatTime defaultTimeLocale "%R %D" t)]

$(viewDataType "Guestbook" "singleEntry")
$(viewDataType "Guestbook" "show")

showXhtml :: [GuestbookEntry] -> StateController GuestbookShow
showXhtml gs = do
  ge <- mapM entryXhtml gs
  return GuestbookShow
    { guestbookSingleEntry = ge
    , title = [H.text "My first Guestbook"]
    , posts = length gs
    }

entryXhtml :: GuestbookEntry -> StateController GuestbookSingleEntry
entryXhtml ge = return GuestbookSingleEntry
    { username = name ge
    , at = createdAt ge
    , msg = message ge
    , delete = [H.textlink ("/guestbook/delete?id=" ++ show (_id ge)) "[Delete]"]
    }

