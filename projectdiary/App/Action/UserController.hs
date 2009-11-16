{-# LANGUAGE TemplateHaskell #-}
module App.Action.UserController where

import App.Action.Common
import App.Model.User
import Hawk.Controller
import Hawk.Model

import qualified System.Log.Logger as Logger
import System.Log.Logger.TH (deriveLoggers)

$(deriveLoggers "Logger" [Logger.DEBUG])

listAction :: StateController [User]
listAction = select newCriteria

editAction :: StateController (User, Bool)
editAction = do
  (user, isNew) <- findOrCreate
  method <- getRequestMethod
  case method of
    POST -> do
      (u, errs) <- updateWithPW user ""
      debugM $ show errs
      if null errs then do
        if isNew then insert u else update u
        setFlash "notice" "Your changes have been saved"
        redirectToAction "user" "list"
        else do
          setErrors "user" errs
          return (u, isNew)
    _ -> return (user, isNew)

updateWithPW :: (HasState m, MonadDB m) => User -> String -> m (User, ValidationErrors)
updateWithPW user prefix = do
  pass <- lookupParam "password"
  conf <- getParam "confirmation"
  case pass of
    Just p -> do
      let pwerrs =  [("password", "must not be null") | null p]
               ++ [("password", "must be confirmed")| p /= conf]
      (u, errs) <- getParams >>= updateAndValidate (user { hashedPassword = hashPassword p }) prefix
      return (u, errs ++ pwerrs)
    -- no password update
    Nothing -> getParams >>= updateAndValidate user prefix

deleteAction :: StateController ()
deleteAction = do
  (user, isNew) <- findOrCreate :: StateController (User, Bool)
  if isNew then setFlash "error" "The requested user does not exist"
    else do
      deleteCascading user
      setFlash "notice" "The user has been deleted"
  redirectToAction "user" "list"

