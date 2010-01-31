-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3
   
   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $
  
   Functions for working with Cookies. This module uses the Cookie functions
   from Network.CGI.Cookies.
   May be updated to reflect RFC 2965 in the future.
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Cookies
  ( Cookie (..)
  , newCookie
  , getCookie
  , getCookieValue
  , setCookie
  , setCookieValue
  , deleteCookie
  , withCookies
  , setTestCookie
  , checkTestCookie
  ) where

import Hawk.Controller.Responses (addHeader)
import Hawk.Controller.Util.List (replaceOrAdd, lookupFirst)
import Hawk.Controller.Types

import Control.Monad (liftM)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks)
import Data.List (intercalate)
import Network.CGI.Cookie
  ( Cookie (..)
  , newCookie
  , showCookie
  , readCookies
  )
import qualified Network.CGI.Cookie as C (deleteCookie)
import Network.HTTP.Headers (HeaderName(..))

testCookie :: (String, String)
testCookie = ("foo", "42")

getRequestCookies :: HasState m => m [Cookie]
getRequestCookies = do
  cookieHeader <- asks $ lookup (show HdrCookie) . http . request
  case cookieHeader of
    Nothing  -> return []
    Just hdr -> return $ map (uncurry newCookie) $ readCookies hdr

getCookie :: HasState m => String -> m (Maybe Cookie)
getCookie name = do
  newCookies <- gets cookies
  oldCookies <- getRequestCookies
  return $ lookupFirst ((==name) . cookieName) (newCookies ++ oldCookies)

getCookieValue :: HasState m => String -> m (Maybe String)
getCookieValue = liftM (liftM cookieValue) . getCookie

setCookie :: HasState m => Cookie -> m ()
setCookie c = modify $ \s -> s { cookies = replaceOrAdd cmp c $ cookies s }
  where cmp c1 c2 = cookieName c1 == cookieName c2

setCookieValue :: HasState m => String -> String -> m ()
setCookieValue name value = do
  old <- getCookie name
  setCookie $ maybe (newCookie name value) (\c -> c { cookieValue = value }) old

deleteCookie :: HasState m => String -> m ()
deleteCookie name = setCookie $ C.deleteCookie $ newCookie name ""

withCookies :: HasState m => m Response -> m Response
withCookies contr = do
  res <- contr
  cs <- gets cookies
  case cs of
    [] -> return res
    cl -> return $ addHeader HdrSetCookie (intercalate ";" $ map showCookie cl) res

setTestCookie :: HasState m => m ()
setTestCookie = uncurry setCookieValue testCookie

checkTestCookie :: HasState m => m Bool
checkTestCookie = do
    t <- getCookieValue (fst testCookie) 
    return $ maybe False ((==) $ snd testCookie) t
