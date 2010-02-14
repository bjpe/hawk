module Hawk.Controller.Auth.EmptyAuth where

import Hawk.Controller.Types
import Hawk.Controller.Auth.ResultType ( AuthResult (..) )

emptyAuth :: AuthType
emptyAuth = return $ AuthFailureUnknown "Empty authentication handler"
