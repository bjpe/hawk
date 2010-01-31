{-# LANGUAGE TemplateHaskell #-}
module App.Action.AdminController where

-- libraries
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import qualified System.Log.Logger as Logger
import System.Log.Logger.TH (deriveLoggers)

-- Hawk
import Hawk.Controller
import Hawk.Model
import qualified Hawk.View.Template.HtmlHelper as Html
import Hawk.View.TemplateView
import Hawk.View.EmptyView

-- App
import App.Action.Common
import App.Action.MainController
import App.Model.User as User
import App.Model.Project as Project

$(deriveLoggers "Logger" [Logger.DEBUG])


{-
  # Login erzwingen für Adminfunktionalitäten
  before_filter :admin_authorize, :except => [:login, :signup]
-}

controllers :: [Routing]
controllers =  xhtmlTemplateView "login" loginAction loginXhtml ++
               xhtmlTemplateView "index" (adminAuthorize indexAction) indexXhtml ++
               xhtml "logout" (adminAuthorize logoutAction) emptyView ++
               [("switchtoproject", (adminAuthorize switchToProjectAction) >>= render emptyView)]

indexAction :: StateController (Integer, Integer, Integer)
indexAction = do
  projectCount <- count (undefined::Project) newCriteria
  adminCount   <- count (undefined::User   ) $ setRestriction (col "admin" `eqExpr` val (1::Int)) newCriteria
  userCount    <- count (undefined::User   ) $ setRestriction (col "admin" `eqExpr` val (0::Int)) newCriteria
  return (projectCount, adminCount, userCount)


loginAction :: StateController ()
loginAction = do
  method <- getRequestMethod
  case method of
    POST -> do
      username <- getParam "username"
      password <- getParam "password"
      test <- User.login username password
      debugM $ show test
      user <- User.adminLogin username password
      case user of
        Nothing -> do
          setFlash "error" "Login failed"
          return ()
        Just u -> do
          setSessionValue "admin_id"   $ show $ User._id  u
          setSessionValue "admin_name"        $ User.name u
          redirectToAction "admin" "index"
    _ -> do
      clearSession
      setFlash "notice" "Welcome! Please login in"


switchToProjectAction :: StateController ()
switchToProjectAction = do
  user <- fromJust `liftM` (readSessionValue "admin_id" >>= liftMaybe findMaybe)
  project' <- readParam "id" >>= liftMaybe findMaybe
  case project' of
    Nothing -> do
      setFlash "error" "The requested project could not be found"
      redirectToAction "admin" "index"
    Just p -> do
      setSessionValue "user_id"    $ show $ User._id    user
      setSessionValue "project_id" $ show $ Project._id p
      setSessionValue "user_name"         $ User.name   user
      redirectToAction "diary" "index"


logoutAction :: StateController ()
logoutAction = redirectToLogin "admin" Nothing


loginXhtml :: Html.XmlTree -> () -> StateController [Html.XmlTree]
loginXhtml template' _ = do
  ah <- adminHeaderXhtml $ head $ subTemplate template' "admin.header"
  return $ bind template'
    [ ("admin.header", ah)
    , ("username", [Html.textfield "username" "" []])
    , ("password", [Html.password  "password" "" []])
    ] []

indexXhtml :: Html.XmlTree -> (Integer, Integer, Integer) -> StateController [Html.XmlTree]
indexXhtml template' (pc,ac,uc) = do
  ah <- adminHeaderXhtml $ head $ subTemplate template' "admin.header"
  return $ bind template'
    [ ("admin.header", ah)
    , ("projectcount", [Html.showtext pc])
    , ("admincount"  , [Html.showtext ac])
    , ("usercount"   , [Html.showtext uc])
    ] []

