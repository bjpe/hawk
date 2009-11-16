-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Core.Responses
   Copyright   :  Copyright (C) 2009 Bj�rn Peem�ller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   A set of standard responses
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Responses
  ( pageResponse
  , cachedResponse
  , redirectResponse
  , forbiddenResponse
  , notFoundResponse
  , errorResponse
  , addDefaultHeaders
  , addCustomHeaders
  , setHeader
  , addHeader
  , setStatus
  , isRedirect
  ) where

import Hack
import Network.HTTP.Headers (HeaderName(..))
import qualified Data.ByteString.Lazy.Char8 as B (length)
import Data.ByteString.Lazy (ByteString)
import Data.Default

setStatus :: Int -> Response -> Response
setStatus code response = response { status = code }

insertHeader :: String -> String -> ((String, String) -> Bool) -> Response -> Response
insertHeader h v p r = r { headers = (h, v) : filter p (headers r) }

setHeader :: HeaderName -> String -> Response -> Response
setHeader h v = insertHeader h' v ((/= h'). fst)
  where h' = show h

addHeader ::HeaderName -> String -> Response -> Response
addHeader h v = insertHeader (show h) v (const True)

addCustomHeaders :: [(String, String)] -> Response -> Response
addCustomHeaders hl r = r { headers = headers r ++ hl }

setBody :: ByteString -> Response -> Response
setBody content r = r { body = content }

htmlResponse :: ByteString -> Response -> Response
htmlResponse content = setHeader HdrContentType "text/html; charset=UTF-8" . setBody content

pageResponse :: ByteString -> Response
pageResponse content =  setStatus 200 $ htmlResponse content def

cachedResponse :: Int -> String -> ByteString -> Response -> Response
cachedResponse age contentType content
  = setStatus 200
  . setHeader HdrCacheControl ("max-age=" ++ show age ++ ", public")
  . setHeader HdrContentType contentType
  . setBody content

redirectResponse :: String -> Response
redirectResponse location = setStatus 302 $ setHeader HdrLocation location def

isRedirect :: Response -> Bool
isRedirect = (== 302) . status

forbiddenResponse :: ByteString -> Response -> Response
forbiddenResponse content = setStatus 403 . htmlResponse content

notFoundResponse :: ByteString -> Response -> Response
notFoundResponse content = setStatus 404 . htmlResponse content

errorResponse :: ByteString -> Response
errorResponse content = setStatus 500 $ htmlResponse content def

addDefaultHeaders :: Response -> Response
addDefaultHeaders response
  = setHeader HdrServer "Hawk"
  $ setHeader HdrContentLength (show $ B.length $ body response) response
