{-# LANGUAGE  TemplateHaskell #-}
-- --------------------------------------------------------------------------
{- |
   Module      :  App.Controller.DiaryController
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Controller for showing and modifying the project diary.
-}
-- --------------------------------------------------------------------------
module App.Action.AreaController where

-- App imports
import App.Action.Common
import App.Action.MainController
import App.Action.PackageController (packageShowXhtml, Package_package, packagePackageXhtml)

import qualified App.Model.Area as Area
import qualified App.Model.Project as Project

-- Hawk imports
import Hawk.Controller
import Hawk.Model
import Hawk.Controller.Util.Monad (liftMaybe)
import qualified Hawk.View.Template.HtmlHelper as Html
import Hawk.View.TemplateView
import Hawk.View.Template.DataType

-- other imports
import Data.Maybe (fromMaybe)

$(viewDataType "Area" "_header")
$(viewDataType "Area" "_area")
$(viewDataType "Area" "show")
$(viewDataTypeWithPrefix "Edit" "Area" "edit")

controllers :: [Routing]
controllers =  xhtmlTypedView "show" (userAuthorize areaShowAction) areaShowXhtml' ++
               xhtmlTypedView "edit" (userAuthorize areaEditAction) areaEditXhtml'


areaShowAction :: StateController Area.Area
areaShowAction = do
  area' <- readParam "id" >>= liftMaybe findMaybe
  case area' of
    Nothing -> do
      setFlash "error" "the requested area could not be found"
      redirectToAction "diary" "index"
    Just a  -> Area.getProject a >>= projectAuthorize >> return a


areaEditAction :: StateController (Area.Area, Bool)
areaEditAction = do
  (area', isNew) <- findOrCreate'
  Area.getProject area' >>= projectAuthorize
  method <- getRequestMethod
  case method of
    POST -> do
      (a, errs) <- getParams >>= updateAndValidate area' ""
      if null errs then do
        if isNew then insert a else update a
        setFlash "notice" "Your changes have been saved"
        redirectToAction "diary" "index"
        else do
          setErrors "area" errs
          return (a, isNew)
    _ -> return (area', isNew)


findOrCreate' :: StateController (Area.Area, Bool)
findOrCreate' = do
  (a, isNew) <- findOrCreate
  if isNew then do
    p <- justProject
    return (a { Area.projectId = Just $ Project._id p }, isNew)
    else return (a, isNew)

-- --------------------------------------------------------------------------
-- Area Views
-- --------------------------------------------------------------------------

areaShowXhtml :: Html.XmlTree -> Area.Area -> StateController [Html.XmlTree]
areaShowXhtml template a = do
      ah <- areaHeaderXhtml a False $ head $ subTemplate template "area.header"
      renderedPackages <- Area.getPackages a >>= concatMapM (packageShowXhtml $ head $ subTemplate template "packages")
      wNow <- Area.workNow a
      wMin <- Area.workMin a
      wMax <- Area.workMax a
      comp <- Area.completeness a
      colA <- Area.color a
      showlink <- actionUrl "area"    "show" [("id", show $ Area._id a)]
      editlink <- actionUrl "area"    "edit" [("id", show $ Area._id a)]
      newlink  <- actionUrl "package" "edit" [("area_id", show $ Area._id a)]
      return $ bind template
        [ ("area.header"     , ah)
        , ("area"            , [Html.textlink showlink (Area.description a)])
        , ("areaWorkMin"     , [Html.showtext wMin])
        , ("areaWorkMax"     , [Html.showtext wMax])
        , ("areaWorkNow"     , [Html.showtext wNow])
        , ("areaCompleteness", [completenessTag colA $ round comp])
        , ("areaNewlink"     , [Html.link newlink  [Html.image "New Package" "/images/new.png" []]])
        , ("areaEditlink"    , [Html.link editlink [Html.image "Edit Area" "/images/edit.png" []]])
        , ("packages"        , renderedPackages)
        ] []

areaShowXhtml' :: Area.Area -> StateController AreaShow
areaShowXhtml' a = do
      ah <- packageHeader a False
      area' <- areaAreaXhtml a
      return AreaShow {
            area_header = [ah],
            area_area = [area']
            }

areaAreaXhtml :: Area.Area -> StateController Area_area
areaAreaXhtml a = do
      renderedPackages <- Area.getPackages a >>= mapM packagePackageXhtml
      wNow <- Area.workNow a
      wMin <- Area.workMin a
      wMax <- Area.workMax a
      comp <- Area.completeness a
      colA <- Area.color a
      showlink <- actionUrl "area"    "show" [("id", show $ Area._id a)]
      editlink <- actionUrl "area"    "edit" [("id", show $ Area._id a)]
      newlink  <- actionUrl "package" "edit" [("area_id", show $ Area._id a)]
      return Area_area {
          area            = [Html.textlink showlink (Area.description a)]
        , areaWorkMin     = [Html.showtext wMin]
        , areaWorkMax     = [Html.showtext wMax]
        , areaWorkNow     = [Html.showtext wNow]
        , areaCompleteness= [completenessTag colA $ round comp]
        , areaNewlink     = [Html.link newlink  [Html.image "New Package" "/images/new.png" []]]
        , areaEditlink    = [Html.link editlink [Html.image "Edit Area" "/images/edit.png" []]]
        , package_package = renderedPackages
        }



areaEditXhtml :: Html.XmlTree -> (Area.Area, Bool) -> StateController [Html.XmlTree]
areaEditXhtml template (a, isNew) = do
     ah <- areaHeaderXhtml a isNew $ head $ subTemplate template "area.header"
     return $ bind template
        [ ("area.header", ah)
        , ("description", [Html.textfield "description" (Area.description a) []])
        , ("comment"    , [Html.textarea  "comment"     (fromMaybe "" $ Area.comment a) []])
        , ("id"         , [Html.hidden    "id"          (show $ Area._id a) [] | not isNew])
        ] []

areaEditXhtml' :: (Area.Area, Bool) -> StateController AreaEdit
areaEditXhtml' (a, isNew) = do
     ah <- packageHeader a isNew
     return AreaEdit {
            editArea_header = [ah],
            editDescription = [Html.textfield "description" (Area.description a) []],
            editComment     = [Html.textarea  "comment"     (fromMaybe "" $ Area.comment a) []],
            editId          = [Html.hidden    "id"          (show $ Area._id a) [] | not isNew]
            }

areaHeaderXhtml :: Area.Area -> Bool -> Html.XmlTree -> StateController [Html.XmlTree]
areaHeaderXhtml area' isNew template = do
    p  <- justProject
    dh <- diaryHeaderXhtml $ head $ subTemplate template "diary.header"
    let path1 = Html.textlink "/diary" (Project.name p) : [Html.text " => "]
    let path2 = if isNew then [Html.text "New"]
                       else [Html.textlink ("/area/show?id=" ++ show (Area._id area')) (Area.description area')]
    return $ bind template
      [ ("diary.header", dh)
      , ("createdAt"   , [Html.text $ maybe "" show $ Area.createdAt area'])
      , ("comment"     , [Html.text $ fromMaybe ""  $ Area.comment area'])
      , ("path"        , path1 ++ path2)
      ] []

packageHeader :: Area.Area -> Bool -> StateController Area_header
packageHeader area' isNew = do
  th <- templatesHeader
  p  <- justProject
  let path1 = Html.textlink "/diary" (Project.name p) : [Html.text " => "]
  let path2 = if isNew then [Html.text "New"]
                       else [Html.textlink ("/area/show?id=" ++ show (Area._id area')) (Area.description area')]
  return Area_header {
    templates_header = th,
    createdAt = [Html.text $ maybe "" show $ Area.createdAt area'],
    comment = [Html.text $ fromMaybe ""  $ Area.comment area'],
    templatesPath = [TemplatesPath $ path1 ++ path2]}
