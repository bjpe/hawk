{-# LANGUAGE TemplateHaskell #-}
module Hawk.Controller.Static
    ( error404
    , error404Response
    , error401
    , error401Response
    , error500
    , error500Response
    , staticRequest
    ) where

import Hawk.Controller.Types
import Hawk.Controller.Responses
import Hawk.Controller.Mime

import System.Directory (doesFileExist, getPermissions, Permissions(readable))
import System.FilePath (takeExtension)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.Lazy as BS (readFile)
import Data.List (isInfixOf)
import Control.Monad.Either
import Control.Monad.Reader (asks)
import Data.Default
import Hawk.Controller.Util.Uri (unescapeUri)

import qualified System.Log.Logger as Logger
import System.Log.Logger.TH (deriveLoggers)

$(deriveLoggers "Logger" [Logger.WARNING])

-- | return a 404 File not found error. I the file could not be found raise a error500
error404 :: EnvController Response
error404 = asks ((++) "/" . error404file . configuration)
    >>= tryStatic
    >>= maybe (warningM "could not find error 404 file" >> error500) (return . setStatus 404)

error404Response :: StateController a
error404Response = lift (lift error404) >>= returnLeft

error401 :: EnvController Response
error401 = asks ((++) "/" . error401file . configuration)
    >>= tryStatic
    >>= maybe (warningM "could not find error 401 file" >> error500) (return . (setStatus 401) . addCustomHeaders [("WWW-Authenticate", "Basic")])

error401Response :: StateController a
error401Response = lift (lift error401) >>= returnLeft

-- | return a 500 internal server error
error500 :: EnvController Response
error500 = asks ((++) "/" . error500file . configuration)
    >>= tryStatic
    >>= maybe (warningM "could not find error 500 file" >> return (errorResponse m)) (return . setStatus 500)
    where
    m = fromString "Internal Server Error 500"

error500Response :: StateController a
error500Response = lift (lift error500) >>= returnLeft

staticRequest :: EnvController Response -> EnvController Response
staticRequest contr = do
  path <- asks $ unescapeUri . pathInfo . request
  isStatic <- tryStatic path
  case isStatic of
    Nothing -> contr
    Just r  -> return r

-- | try to deliver static content from the public dir
tryStatic :: String -> EnvController (Maybe Response)
tryStatic path =
  if ".." `isInfixOf` path -- TODO make a real check if dir is sub dir
    then return $ Just $ forbiddenResponse (fromString path) def
    else do
      let path' = if path == "/" then "/index.xhtml" else path
      pDir <- asks $ publicDir . configuration
      let fullPath = pDir ++ path'
      fileExists <- liftIO $ doesFileExist fullPath
      if fileExists
        then do
          canRead <- liftIO $ liftM readable $ getPermissions fullPath
          if canRead
            then do
              file <- liftIO $ BS.readFile fullPath
              return $ Just $ cachedResponse 600 (getMimeType $ takeExtension fullPath) file def
            else return $ Just $ forbiddenResponse (fromString path) def
        else return Nothing
