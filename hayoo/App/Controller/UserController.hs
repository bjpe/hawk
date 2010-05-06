{-# LANGUAGE TemplateHaskell #-}
module App.Controller.UserController where

import Hawk.Controller
import Hawk.View
import Hawk.Model
import Hawk.Controller.Auth.ResultType
--import Hawk.Controller.Static

import App.View.UserView
import App.Model.User as U
import App.HolWrapper.Common
-- import App.HolumbusWrapper.QuerySettingsHelper (toInt, toFloat)

import qualified Data.Map as M

import Control.Monad (liftM)
--import Control.Monad.Error (catchError)

import qualified System.Log.Logger as Logger
import System.Log.Logger.TH ( deriveLoggers )

$(deriveLoggers "Logger" [Logger.DEBUG])

routes :: [Routing]
routes = 
  [ ("register",registerAction >>= render (typedView "register" registerXhtml))
  , ("login",authAction >> redirectToAction "index" "index")
  ] ++ combine (authF' "You are not logged in." "index" "index") 
  -- following actions require a logged in user, else he'll be redirected to index page
  [ ("index",indexAction >>= render (typedView "index" indexXhtml))
  , ("show",indexAction >>= render (typedView "index" indexXhtml))
  , ("edit",editAction >> redirectToAction "user" "index")
  , ("logout",logoutAction >> redirectToAction "index" "index")
  , ("delete",deleteAction >> redirectToAction "user" "logout")
  ]

indexAction :: StateController User
indexAction = getCurUser

editAction :: StateController ()
editAction = do
  method <- getRequestMethod
  case method of
    POST -> do
      let errs = ""
      user <- getCurUser
      debugM $ show user
--      (u, errs) <- getParams >>= updateAndValidate user "" -- checkboxes will not be updated
      u <- getParams >>= myUpdateByParams user
      if null errs 
        then do
          debugM $ show u
          _ <- update u
          setFlash "success" "Successfully changed your Settings."
          return ()
        else do
          setFlash "error" $ show errs
          --setErrors "userEdit" errs
    _ -> return ()

registerAction :: StateController ()
registerAction = do
  u <- getParam "username"
  if null u
    then return () -- showRegistrationForm
    else do
      n <- new :: StateController User
      (user, errs) <- getParams >>= updateAndValidate n ""
      if null errs
        then do
          _ <- insert user
          p <- getParam "password"
          setFlash "success" ("You are successfully registered as " ++ u)
          redirectTo "user" "login" [("username",u),("password",p)]
        else setErrors "registerUser" errs -- registration failed, show form

authAction :: StateController ()
authAction = tryLogin >>= flashAuth
{-  -- http basic auth
  u <- lookupParam "username"
  p <- lookupParam "password"
  r <- case (u,p) of
         (Just user, Just pass) -> A.tryLogin user pass
         _ -> A.tryLogin "" ""
  case r of
    AuthSuccess -> setFlash "success" "Authentication succeeded." 
    _ -> error401Response -- this overwrites the redirectToAction from the routing list -}
  -- db auth
--  r <- tryLogin
--  flashAuth r

logoutAction :: StateController ()
logoutAction = logout

deleteAction :: StateController ()
deleteAction = do 
  res <- getCurUser >>= delete
  if res then setFlash "success" "Your account were deleted successfully."
         else setFlash "error" "Failed to delete your account, please contact us."

-- ############## private

flashAuth :: AuthResult -> StateController ()
flashAuth AuthSuccess = setFlash "success" "Authentication succeeded."
flashAuth AuthFailureIdNotFound = setFlash "error" "Username not found."
flashAuth AuthFailureInvalidCredential = setFlash "error" "Wrong password."
flashAuth _ = setFlash "error" "An unknown error occurred while login."

getCurUser :: StateController User
getCurUser = isAuthedAs >>= (\u -> selectOne $ restrictionCriteria $ (val u) .==. (col "username"))

-- | You can call this when you've successfully loaded an user
addToSession :: User -> StateController ()
addToSession u = head `liftM` (mapM (uncurry (setSessionValue)) $ U.toList u)


myUpdateByParams :: User -> M.Map String String -> StateController User
myUpdateByParams u m = return $ u 
      { --username = user -- not changeable
      --, password = pass
      --, email = mail
       caseSensitive = maybe (caseSensitive u) (Just . toBool) $ M.lookup "caseSensitive" m
      , optimizeQuery = maybe (optimizeQuery u) toBool $ M.lookup "optimizeQuery" m
      , wordLimit = maybe (wordLimit u) toInt $ M.lookup "wordLimit" m
      , replace = maybe (replace u) toBool $ M.lookup "replace" m
      , swapChars = maybe (swapChars u) toBool $ M.lookup "swapChars" m
      , replacements = maybe (replacements u) justStr $ M.lookup "replacements" m
      , maxFuzzy = maybe (maxFuzzy u) (realToFrac . toFloat) $ M.lookup "maxFuzzy" m
      , modules = maybe (modules u) justStr $ M.lookup "modules" m
      , packages = maybe (packages u) justStr $ M.lookup "packages" m
      } 
      where justStr [] = Nothing
            justStr s = Just s
