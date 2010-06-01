module Hawk.Controller.Auth.EmptyAuth where

import Hawk.Controller.Types

emptyAuth :: AuthType
emptyAuth = return $ AuthFailureUnknown "Empty authentication handler"
