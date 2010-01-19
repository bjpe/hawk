module Hawk.Controller.Session.UrlSession where
--session id will be transmitted by url get parameters

import Hawk.Controller.Types
import Hawk.Controller.Session

urlSession :: SessionStore
urlSession = SessionStore
  { -- readSession :: SessionOpts -> m Session
    readSession = readUrlSession
    -- saveSession :: SessionOpts -> Session -> m ()
  , saveSession = saveUrlSession
  }

newUrlSession :: HasState m => SessionOpts -> m Session
newUrlSession opts = 

readUrlSession :: (MonadIO m, HasState m) => SessionOpts -> m Session
readUrlSession opts = do
  s <- lookupParam $ fromJust . sessionParam
  case s of
    Nothing -> newUrlSession opts
    Just v  -> 

saveUrlSession :: HasState m => SessionOpts -> Session -> m ()
saveUrlSession opts sess = undefined

sessionParam :: SessionOpts -> Maybe String
sessionParam opts = lookup "sessionParam"
