module App.Controller.UserController where

import Hawk.Controller
import Hawk.View
import Hawk.Model
import Hawk.Controller.Auth.ResultType
import Hawk.Controller.Static

import Hawk.Controller
import Hawk.View

import App.View.UserView
import App.Model.User

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
  ]

indexAction :: StateController ()
indexAction = return () -- TODO load user configuration from db

editAction :: StateController ()
editAction = return () -- TODO try to set new user configuration and redirect to user/index

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
{- http basic auth
  u <- lookupParam "username"
  p <- lookupParam "password"
  r <- case (u,p) of
         (Just user, Just pass) -> A.tryLogin user pass
         _ -> A.tryLogin "" ""
  case r of
    AuthSuccess -> setFlash "success" "Authentication succeeded." 
    _ -> error401Response -- this overwrites the redirectToAction from the routing list -}
  u <- lookupParam "username"
  p <- lookupParam "password"
  case (u,p) of
    (Just user, Just pass) -> do
      r <- tryLogin user pass
      flashAuth r
    _ -> setFlash "error" "No username or password entered."

logoutAction :: StateController ()
logoutAction = logout

-- ############## private

flashAuth :: AuthResult -> StateController ()
flashAuth AuthSuccess = setFlash "success" "Authentication succeeded."
flashAuth AuthFailureIdNotFound = setFlash "error" "Username not found."
flashAuth AuthFailureInvalidCredential = setFlash "error" "Wrong password."
flashAuth _ = setFlash "error" "An unknown error occurred while login."

