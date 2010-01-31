{-# LANGUAGE  TemplateHaskell #-}
module App.View.DiaryView where

import App.Action.Common
import App.Action.AreaController (Area_area, areaShowXhtml, areaAreaXhtml)
import App.Action.DiaryController
import App.Action.MainController
import qualified App.Model.Project as Project
import qualified App.Model.User as User
import Hawk.Controller
import Hawk.Model
import Hawk.Controller.Util.Monad
import qualified Hawk.View.Template.HtmlHelper as Html
import Hawk.View.TemplateView as TV
import Hawk.View.Template.DataType
import Hawk.View.EmptyView

import Data.List (intercalate)
import Control.Monad (liftM)

$(viewDataType "Diary" "_header")
$(viewDataType "Diary" "index")
$(viewDataTypeWithPrefix "Login" "Diary" "login")
$(viewDataTypeWithPrefix "Edit" "Diary" "edit")

views :: [Routing]
views =  [ ("login",  loginAction >>= render (typedView "login" loginXhtml'))
         , ("logout", userAuthorize logoutAction >>= render emptyView)
         , ("index" , userAuthorize indexAction >>= render (typedView "index" indexXhtml'))
         , ("edit"  , userAuthorize editAction >>= render (typedView "edit" editXhtml'))
         , ("help"  , return () >>= render (TemplateView "help" helpXhtml))
         ]


loginXhtml :: XmlTree -> t -> StateController [XmlTree]
loginXhtml template _ = do
  projects <- select newCriteria
  diaryHeader' <- diaryHeaderXhtml $ head $ template `subTemplate` "diary.header"
  return $ bind template
    [ ("diary.header", diaryHeader')
    , ("projects", [Html.select    "project.id" (Html.options (show . Project._id) Project.name projects) []])
    , ("user"    , [Html.textfield "user" "" []])
    , ("password", [Html.password  "password" "" []])
    , ("login"   , [Html.submit    "Login" []])
    , ("path"    , [ Html.textlink "/admin/login" "Admin section"
                   , Html.text " | "
                   , Html.textlink "/diary/help" "Help"
                   ])
    ] []

loginXhtml' :: t -> StateController DiaryLogin
loginXhtml' _ = do
  projects <- select newCriteria
  return DiaryLogin
    { loginProjects= [Html.select    "project.id" (Html.options (show . Project._id) Project.name projects) []]
    , loginUser    = [Html.textfield "user" "" []]
    , loginPassword= [Html.password  "password" "" []]
    , loginLogin   = [Html.submit    "Login" []]
    , loginTemplatesPath = [TemplatesPath [ Html.textlink "/admin/login" "Admin section"
                   , Html.text " | "
                   , Html.textlink "/diary/help" "Help"
                   ]]
    }


{-
indexXhtml template p = do
  let renderedAreas t = Project.getAreas p >>= concatMapM (areaShowXhtml t)
  wMin <- Project.workMin p
  wMax <- Project.workMax p
  wNow <- Project.workNow p
  c    <- Project.completeness p
  bindDiaryIndex template $ DiaryIndex
    { indexlink = Html.textlink "/diary" $ Project.name p
    , workMin = Html.showtext wMin
    , workMax = Html.showtext wMax
    , workNow = Html.showtext wNow
    , projectCompleteness = formatCompleteness c
    , areas = renderedAreas
    }
-}

indexXhtml :: XmlTree -> Project.Project -> StateController [XmlTree]
indexXhtml template p = do
  header' <- diaryHeaderXhtml $ head $ template `subTemplate` "header"
  renderedAreas <- Project.getAreas p >>= concatMapM
      (areaShowXhtml $ head $ subTemplate template "areas")
  wMin <- Project.workMin p
  wMax <- Project.workMax p
  wNow <- Project.workNow p
  c    <- Project.completeness p
  pCol <- Project.color p
  return $ bind template
    [ ("header"      , header')
    , ("indexlink"   , [Html.textlink "/diary" (Project.name p)])
    , ("workMin"     , [Html.showtext wMin])
    , ("workMax"     , [Html.showtext wMax])
    , ("workNow"     , [Html.showtext wNow])
    , ("completeness", [completenessTag pCol $ round c])
    , ("areas"       , renderedAreas)
    ] []

indexXhtml' :: Project.Project -> StateController DiaryIndex
indexXhtml' p = do
  header <- diaryHeader
  renderedAreas <- Project.getAreas p >>= mapM areaAreaXhtml
  wMin <- Project.workMin p
  wMax <- Project.workMax p
  wNow <- Project.workNow p
  c    <- Project.completeness p
  pCol <- Project.color p
  return DiaryIndex {
      diary_header= header
    , indexlink   = [Html.textlink "/diary" (Project.name p)]
    , workMin     = [Html.showtext wMin]
    , workMax     = [Html.showtext wMax]
    , workNow     = [Html.showtext wNow]
    , diaryCompleteness= [completenessTag pCol $ round c]
    , area_area   = renderedAreas
    }

editXhtml :: XmlTree -> Project.Project -> StateController [XmlTree]
editXhtml template p = do
  header <- diaryHeaderXhtml $ head $ template `subTemplate` "header"
  return $ bind template
    [ ("header"     , header)
    , ("description", [Html.textfield "project.description" (Project.description p) []])
    , ("startAt"    , Html.selectDate "project.startAt"     (Project.startAt p) Html.monthNames [])
    , ("endAt"      , Html.selectDate "project.endAt"       (Project.endAt p) Html.monthNames [])
    ] []

editXhtml' :: Project.Project -> StateController DiaryEdit
editXhtml' p = do
  header <- diaryHeader
  return DiaryEdit
    { editDiary_header = header
    , editDescription  = [Html.textfield "project.description" (Project.description p) [("maxlength","64")]]
    , editStartAt      = Html.selectDate "project.startAt"     (Project.startAt p) Html.monthNames []
    , editEndAt        = Html.selectDate "project.endAt"       (Project.endAt p) Html.monthNames []
    }


helpXhtml :: XmlTree -> t -> StateController [XmlTree]
helpXhtml template _ = do
  diaryHeader' <- diaryHeaderXhtml $ head $ template `subTemplate` "diary.header"
  project' <- currentProject
  let path' = case project' of
              Just p  -> [Html.textlink "/diary" $ Project.name p]
              Nothing -> [Html.textlink "/diary/login" "Login"]
  return $ bind template
    [ ("diary.header", diaryHeader')
    , ("path"        , path')
    ] []

headerXhtml :: XmlTree -> Project.Project -> StateController [XmlTree]
headerXhtml template p = do
  diaryHeader' <- diaryHeaderXhtml $ head $ template `subTemplate` "diary.header"
  return $ bind template
    [ ("diary.header", diaryHeader')
    , ("path"        , [Html.textlink "/diary" $ Project.name p])
    ] []

diaryHeader :: StateController [Diary_header]
diaryHeader = do
  pid      <- readSessionValue "project_id" >>= liftMaybe findMaybe
  case pid of
    Nothing -> return []
    Just p -> do
      templatesHeader' <- templatesHeader
      workers'  <- liftM (Html.text . intercalate ", " . map User.name) (Project.workingUsers p)
      return [Diary_header {
        templates_header = templatesHeader'
        , templatesPath  = [TemplatesPath {path = [Html.textlink "/diary" $ Project.name p]}]
        , startAt        = [Html.showtext $ Project.startAt p]
        , endAt          = [Html.showtext $ Project.endAt   p]
        , workers        = [workers']
        }]
