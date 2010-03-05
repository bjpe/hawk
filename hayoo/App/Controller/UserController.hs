{-# LANGUAGE TemplateHaskell #-}
module App.Controller.UserController where

import Hawk.Controller
import Hawk.View
import Hawk.Model
import Hawk.Controller.Auth.ResultType
import Hawk.Controller.Static

import Hawk.Controller
import Hawk.View

import App.View.UserView
import App.Model.User as U
import App.HolumbusWrapper.QuerySettingsHelper (toIgr)

import qualified Data.Map as M
import Control.Monad (liftM)

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
  , ("delete",deleteAction >> redirectToAction "index" "index")
  ]

indexAction :: StateController User
indexAction = isAuthedAs >>= (\u -> selectOne $ restrictionCriteria $ (val u) .==. (col "username"))

editAction :: StateController ()
editAction = do --return () -- TODO try to set new user configuration and redirect to user/index
  method <- getRequestMethod
  case method of
    POST -> do
      params <- getParams
      n <- (findMaybe $ toIgr $ M.findWithDefault "-1" "uid" params)::StateController (Maybe User)
      case n of
        Nothing -> setFlash "error" "An error occurred." >> redirectToAction "user" "logout"
        Just v -> do
          (u, errs) <- getParams >>= updateByParams v ""
          if null errs 
            then do
              update u
              setFlash "success" "Successfully changes your Settings."
            else do
              setFlash "error" $ show errs
              setErrors "userEdit" errs
          return ()
    _ -> return ()

registerAction :: StateController ()
registerAction = do
  u <- getParam "username"
  if null u
    then return () -- showRegistrationForm
    else do
      new <- new :: StateController User
      (user, errs) <- getParams >>= updateAndValidate new ""
      if null errs
        then do
          insert user
          p <- getParam "password"
          setFlash "success" ("You are successfully registered as " ++ u)
          redirectTo "user" "login" [("username",u),("password",p)]
        else setErrors "registerUser" errs -- registration failed, show form

authAction :: StateController ()
authAction = do
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
  r <- tryLogin
  flashAuth r

logoutAction :: StateController ()
logoutAction = logout

deleteAction :: StateController ()
deleteAction = return ()

-- ############## private

flashAuth :: AuthResult -> StateController ()
flashAuth AuthSuccess = setFlash "success" "Authentication succeeded."
flashAuth AuthFailureIdNotFound = setFlash "error" "Username not found."
flashAuth AuthFailureInvalidCredential = setFlash "error" "Wrong password."
flashAuth _ = setFlash "error" "An unknown error occurred while login."

