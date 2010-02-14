module App.View.AjaxView where

import Hawk.Controller
import Hawk.View

--import Data.ByteString.UTF8 (ByteString)

indexJson :: JSON -> StateController ByteString
indexJson s = return $ jsonEncode s
