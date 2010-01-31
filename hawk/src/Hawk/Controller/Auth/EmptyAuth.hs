module Hawk.Controller.Auth.EmptyAuth where

import Hawk.Controller.Types
import Hawk.Controller.Auth.ResultType ( AuthResult (..) )

emptyAuth :: AuthType
emptyAuth = AuthType
  { -- authenticate :: a -> m AuthResult
    authenticate = \_ _ -> return $ AuthFailureUnknown "Empty authentication handler"
  }
