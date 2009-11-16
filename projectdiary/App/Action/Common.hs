{-# LANGUAGE  TemplateHaskell #-}
module App.Action.Common where

import qualified App.Model.User as User
import qualified App.Model.Project as Project

import Hawk.Controller
import Hawk.Model
import Hawk.Controller.Util.Monad
import Hawk.View.Template.HtmlHelper as Html
import Hawk.View.TemplateView
import Hawk.View.Template.DataType

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

$(viewDataType "Templates" "path")
$(viewDataType "Templates" "_header")

findOrCreate :: Model a => StateController (a, Bool)
findOrCreate = do
  model <- readParam "id" >>= liftMaybe findMaybe
  case model of
    Nothing -> do
      newModel <- new
      return (newModel, True )
    Just m  -> return (m  , False)


diaryHeaderXhtml :: XmlTree -> StateController [XmlTree]
diaryHeaderXhtml template = do
  pid      <- readSessionValue "project_id" >>= liftMaybe findMaybe
  case pid of
    Nothing -> return $ bind template [("noSession", [])] []
    Just p  -> do
      username <- getSessionValue "user_name"
      c        <- round `liftM` Project.completeness p :: StateController Integer
      workers  <- Project.workingUsers p
      return $ bind template
        [ ("project",      [Html.text $ Project.name p])
        , ("completeness", [Html.showtext c])
        , ("name",         [Html.text $ fromMaybe "" username])
        , ("logout",       [Html.link "/diary/logout" [Html.image "Logout" "/images/logout.png" []]])
        , ("startAt",      [Html.showtext $ Project.startAt p])
        , ("endAt",        [Html.showtext $ Project.endAt p])
        , ("workers",      [Html.text $ intercalate ", " $ map User.name workers])
        , ("path",         [Html.textlink "/diary" $ Project.name p])
        ] []


adminHeaderXhtml :: Html.XmlTree -> StateController [Html.XmlTree]
adminHeaderXhtml template = do
  username <- getSessionValue "admin_name"
  case username of
    Nothing -> return $ bind template
      [ ("withSession", [])
      , ("path"       , [Html.textlink "/diary/login" "Diary"])
      ] []
    Just u  -> return $ bind template
      [ ("path", [ Html.textlink "/admin/index" "Home"
                 , Html.text " | "
                 , Html.textlink "/project/list" "Projects"
                 , Html.text " | "
                 , Html.textlink "/user/list" "Users"
                 , Html.text " | "
                 , Html.textlink "/diary/login" "Diary"
                 ])
      , ("name"  , [Html.text u])
      , ("logout", [Html.link "/admin/logout" [Html.image "Logout" "/images/logout.png" []]])
      ] []

templatesHeader :: StateController [Templates_header]
templatesHeader = do
  pid      <- readSessionValue "project_id" >>= liftMaybe findMaybe
  case pid of
    Nothing -> return []
    Just p  -> do
      username <- getSessionValue "user_name"
      c        <- round `liftM` Project.completeness p :: StateController Integer
      -- workers  <- Project.workingUsers p
      return [Templates_header {
                     name = [Html.text $ fromMaybe "" username]
                     , project = Project.name p
                     , logout = [Html.link "/diary/logout" [Html.image "Logout" "/images/logout.png" []]]
                     , completeness = [Html.showtext c]}]
{-
 bind template
        [ ("project",      [Html.text $ Project.name p])
        , ("completeness", [Html.showtext c])
        , ("name",         [Html.text $ fromMaybe "" username])
        , ("logout",       [Html.link "/diary/logout" [Html.image "Logout" "/images/logout.png" []]])
        , ("startAt",      [Html.showtext $ Project.startAt p])
        , ("endAt",        [Html.showtext $ Project.endAt p])
        , ("workers",      [Html.text $ intercalate ", " $ map User.name workers])
        , ("path",         [Html.textlink "/diary" $ Project.name p])
        ] []-}

completenessTag :: String -> Integer -> XmlTree
completenessTag _     0       = Html.text "0%"
completenessTag statuscolor percentage
  = Html.contentTag "div"
    [ ("align", "center"), ("style", "width:" ++ show percentage ++ "%")
    , ("title", show percentage ++ " percent"), ("class", statuscolor)
    ]
    [Html.text $ show percentage ++ "%"]

type CompTag = (String, Integer)

completenessTag' :: CompTag -> [XmlTree]
completenessTag' (s,i) = [completenessTag s i]


{-
<%= link_to "Home", :controller => "admin", :action => "index" %>
	  &nbsp; | &nbsp;
	  <%= link_to "Projekte", :controller => "project"%>
	  <%= link_to "Benutzer", :controller => "user" %>
	  &nbsp; | &nbsp;
	  <%= link_to "Tagebuchbereich", :controller => "tagebuch", :action => "login" %>	
-}
