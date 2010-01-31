-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Core.Session.CookieSession
   Copyright   :  Copyright (C) 2009 Bj�rn Peem�ller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Saving a session in cookies. Code is adapted from Alson Kemp's Turbinado.
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Session.CookieSession (cookieStore) where

import Hawk.Controller.Types
import Hawk.Controller.Session
import Hawk.Controller.Cookies
import Hawk.Controller.Util.Read (maybeRead)
import Data.Digest.Pure.SHA (hmacSha256, bytestringDigest)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.ByteString.Lazy (unpack, append)
import Data.Maybe (fromMaybe, fromJust)
import qualified Codec.Binary.Base64Url as Base64 (encode, decode)
import qualified Codec.Binary.UTF8.String as UTF8 (decode)
import Text.Regex.Posix (getAllTextSubmatches, (=~))
import Control.Monad.Trans (MonadIO)

secretMinLength :: Int
secretMinLength = 32 -- bytes
-- In rfc2104 a length of L strong random chars is suggested
-- L is the length of the hash output (for sha1 20; for sha224 28; for sha256 32)
-- a longer Key does not make the hmac more secure, becuase the key is hashed

cookieStore :: SessionStore
cookieStore = SessionStore
  { readSession   = readCookieSession
  , saveSession   = saveCookieSession
  }

newCookieSession :: HasState m => SessionOpts -> m Session
newCookieSession opts =
  case secret opts of
    Nothing -> error "no secret in the cookie session options"
    Just s  -> if length s < secretMinLength then
                 error $ "secret must contain at least " ++ show secretMinLength ++ " characters"
                 else return $ emptySession $ sessionCookieName opts

readCookieSession :: (MonadIO m, HasState m) => SessionOpts -> m Session
readCookieSession opts = do
    sessionCookie <- getCookieValue $ sessionCookieName opts
    case sessionCookie >>= unmarshal (fromJust $ secret opts) >>= maybeRead of
      Nothing -> newCookieSession opts
      Just s -> do
        e <- expired s
        if not e
           then return s
           else newCookieSession opts

saveCookieSession :: HasState m => SessionOpts -> Session -> m ()
saveCookieSession opts sess =
  setCookie $ (newCookie (sessionId sess) marshalled) { cookiePath = Just "/" }
  where marshalled = marshal (fromJust $ secret opts) (show sess)

marshal :: String -> String -> String
marshal key s = toString $ encoded `append` fromString "--" `append` digest (fromString key) encoded
  where encoded = encode $ fromString s

-- TODO move to ByteString
unmarshal :: String -> String -> Maybe String
unmarshal key s
  | toString (digest (fromString key) (fromString content)) == sig = decode content
  | otherwise               = Nothing
  where
    [_, content, sig] = getAllTextSubmatches $ s =~ "^(.*)--(.*)$"

-- QuickCheck Idea: x == unmarshal k (marshal k x)

-- | Use a hmac to sign the Message with Key, the output is the base64url encoded hmac
digest :: ByteString -> ByteString -> ByteString
digest key = encode . bytestringDigest . hmacSha256 key

-- | Encode a UTF8 String to a Base64
encode :: ByteString -> ByteString
encode = fromString . Base64.encode . unpack

-- | Decode a Base64 String to a UTF8 String
decode :: String -> Maybe String
decode s = Base64.decode s >>= Just . UTF8.decode
--decode :: String -> Maybe ByteString
--decode s = Base64.decode s >>= Just . pack

secret :: SessionOpts -> Maybe String
secret = lookup "secret"

sessionCookieName :: SessionOpts -> String
sessionCookieName = fromMaybe defaultName . lookup "cookie-name"
