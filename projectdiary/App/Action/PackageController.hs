{-# LANGUAGE  TemplateHaskell #-}
-- --------------------------------------------------------------------------
{- |
   Module      :  App.Controller.PackageController
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Controller for showing and modifying packages
-}
-- --------------------------------------------------------------------------
module App.Action.PackageController where

-- App imports
import App.Action.Common
import App.Action.MainController
import App.Action.StepController (stepShowXhtml, stepStepXhtml, Step_step)
import qualified App.Model.Package as Package
import qualified App.Model.Area as Area
import qualified App.Model.Project as Project

-- Hawk imports
import Hawk.Controller
import Hawk.Model
import Hawk.Controller.Util.Monad (liftMaybe)
import qualified Hawk.View.Template.HtmlHelper as Html
import Hawk.View.TemplateView
import Hawk.View.Template.DataType

-- other stuff
import Control.Monad (liftM)
import Data.Maybe (fromMaybe, fromJust)


$(viewDataType "Package" "_header")
$(viewDataType "Package" "_package")
$(viewDataType "Package" "show")
$(viewDataTypeWithPrefix "Edit" "Package" "edit")

controllers :: [Routing]
controllers =  [ ("show", userAuthorize packageShowAction >>= render (typedView "show" packageShowXhtml'))
               , ("edit", userAuthorize packageEditAction >>= render (typedView "edit" packageEditXhtml'))
               ]


packageShowAction :: StateController Package.Package
packageShowAction = do
  package' <- readParam "id" >>= liftMaybe findMaybe
  case package' of
    Nothing -> do
      setFlash "error" "the requested area could not be found"
      redirectToAction "diary" "index"
    Just p  -> Package.getProject p >>= projectAuthorize >> return p


packageEditAction :: StateController (Package.Package, Bool)
packageEditAction = do
  (package', isNew) <- findOrCreate'
  Package.getProject package' >>= projectAuthorize
  method <- getRequestMethod
  case method of
    POST -> do
      (p, errs) <- getParams >>= updateAndValidate package' ""
      if null errs then do
        if isNew then insert p else update p
        setFlash "notice" "Your changes have been saved"
        redirectToAction "diary" "index"
        else do
          setErrors "package" errs
          return (p, isNew)
    _ -> return (package', isNew)


findOrCreate' :: StateController (Package.Package, Bool)
findOrCreate' = do
  (p, isNew) <- findOrCreate
  if isNew then do
    area <- readParam "area_id" >>= liftMaybe findMaybe
    case area of
      Nothing -> do
        setFlash "error" "the requested area could not be found"
        redirectToAction "diary" "index"
      Just a -> return (p { Package.areaId = Just $ Area._id a }, isNew)
    else return (p, isNew)


packageShowXhtml :: Html.XmlTree -> Package.Package -> StateController [Html.XmlTree]
packageShowXhtml template p = do
  ph <- packageHeaderXhtml p False $ head $ subTemplate template "package.header"
  renderedSteps <- Package.getSteps p >>= concatMapM (stepShowXhtml $ head $ subTemplate template "steps")
  wNow    <- Package.workNow p
  c       <- fromIntegral `liftM` Package.completeness p
  pColor  <- Package.color p
  return $ bind template
    [ ("package.header" , ph)
    , ("package"        , [Html.textlink ("/package/show?id=" ++ show (Package._id p)) (Package.description p)])
    , ("packageWorkMin" , [Html.showtext $ Package.workMin p])
    , ("packageWorkMax" , [Html.showtext $ Package.workMax p])
    , ("packageWorkNow" , [Html.showtext wNow])
    , ("packageCompleteness", [completenessTag pColor c])
    , ("packageNewlink" , [Html.link ("/step/edit?package_id=" ++ show (Package._id p)) [Html.image "New Step" "/images/new.png" []]])
    , ("packageEditlink", [Html.link ("/package/edit?id=" ++ show (Package._id p)) [Html.image "Edit package" "/images/edit.png" []]])
    , ("steps"          , renderedSteps)
    ] []

packageShowXhtml' :: Package.Package -> StateController PackageShow
packageShowXhtml' p = do
  ph <- packageHeader p False
  package' <- packagePackageXhtml p
  return PackageShow {
        package_header = [ph],
        package_package = [package']
        }

packagePackageXhtml :: Package.Package -> StateController Package_package
packagePackageXhtml p = do
    step_steps <- liftM (map stepStepXhtml) (Package.getSteps p)
    wNow    <- Package.workNow p
    c       <- fromIntegral `liftM` Package.completeness p
    pColor  <- Package.color p
    return Package_package {
        package = [Html.textlink ("/package/show?id=" ++ show (Package._id p)) (Package.description p)],
        packageWorkMin = [Html.showtext $ Package.workMin p],
        packageWorkMax = [Html.showtext $ Package.workMax p],
        packageWorkNow = [Html.showtext wNow],
        packageCompleteness = [completenessTag pColor c],
        packageNewlink = [Html.link ("/step/edit?package_id=" ++ show (Package._id p)) [Html.image "New Step" "/images/new.png" []]],
        packageEditlink = [Html.link ("/package/edit?id=" ++ show (Package._id p)) [Html.image "Edit package" "/images/edit.png" []]],
        step_step = step_steps
        }

packageEditXhtml :: Html.XmlTree -> (Package.Package, Bool) -> StateController [Html.XmlTree]
packageEditXhtml template (p, isNew) = do
     ph <- packageHeaderXhtml p isNew $ head $ subTemplate template "package.header"
     return $ bind template
        [ ("package.header", ph)
        , ("description"   , [Html.textfield "description" (Package.description p) []])
        , ("comment"       , [Html.textarea  "comment"     (fromMaybe "" $ Package.comment p) []])
        , ("workMin"       , [Html.textfield "workMin"     (show $ Package.workMin p) []])
        , ("workMax"       , [Html.textfield "workMax"     (show $ Package.workMax p) []])
        , ("area_id"       , [Html.hidden    "area_id"     (show $ fromJust $ Package.areaId p) [] | isNew])
        , ("id"            , [Html.hidden    "id"          (show $ Package._id p) [] | not isNew])
        ] []


packageEditXhtml' :: (Package.Package, Bool) -> StateController PackageEdit
packageEditXhtml' (p, isNew) = do
     ph <- packageHeader p isNew
     return PackageEdit {
            editPackage_header = [ph],
            editDescription = [Html.textfield "description" (Package.description p) []],
            editComment     = [Html.textarea  "comment"     (fromMaybe "" $ Package.comment p) []],
            editWorkMin     = [Html.textfield "workMin"     (show $ Package.workMin p) []],
            editWorkMax     = [Html.textfield "workMax"     (show $ Package.workMax p) []],
            editArea_id     = [Html.hidden    "area_id"     (show $ fromJust $ Package.areaId p) [] | isNew],
            editId          = [Html.hidden    "id"          (show $ Package._id p) [] | not isNew]
            }


packageHeaderXhtml :: Package.Package -> Bool -> Html.XmlTree -> StateController [Html.XmlTree]
packageHeaderXhtml package' isNew template = do
  dh <- diaryHeaderXhtml $ head $ subTemplate template "diary.header"
  a  <- Package.getArea package'
  p  <- Area.getProject a
  let path1 = [Html.textlink "/diary" (Project.name p)] ++ [Html.text " => "]
           ++ [Html.textlink ("/area/show?id=" ++ show (Area._id a)) (Area.description a)] ++ [Html.text " => "]
  let path2 = if isNew then [Html.text "New"]
                       else [Html.textlink ("/package/show?id=" ++ show (Package._id package')) (Package.description package')]
  return $ bind template
    [ ("diary.header", dh)
    , ("createdAt"   , [Html.text $ maybe "" show $ Package.createdAt package'])
    , ("comment"     , [Html.text $ fromMaybe ""  $ Package.comment package'])
    , ("path"        , path1 ++ path2)
    ] []

packageHeader :: Package.Package -> Bool -> StateController Package_header
packageHeader package' isNew = do
  th <- templatesHeader
  a  <- Package.getArea package'
  p  <- Area.getProject a
  let path1 = [Html.textlink "/diary" (Project.name p)] ++ [Html.text " => "]
           ++ [Html.textlink ("/area/show?id=" ++ show (Area._id a)) (Area.description a)] ++ [Html.text " => "]
  let path2 = if isNew then [Html.text "New"]
                       else [Html.textlink ("/package/show?id=" ++ show (Package._id package')) (Package.description package')]
  return Package_header {
    templates_header = th,
    createdAt = [Html.text $ maybe "" show $ Package.createdAt package'],
    comment = [Html.text $ fromMaybe ""  $ Package.comment package'],
    templatesPath = [TemplatesPath $ path1 ++ path2]}
