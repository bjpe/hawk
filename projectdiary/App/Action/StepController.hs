{-# LANGUAGE  TemplateHaskell #-}
-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Controller for showing and modifying steps
-}
-- --------------------------------------------------------------------------
module App.Action.StepController where

-- App imports
import App.Action.Common
import App.Action.MainController
import qualified App.Model.Step as Step
import qualified App.Model.User as User
import qualified App.Model.Project as Project
import qualified App.Model.Area as Area
import qualified App.Model.Package as Package

-- Hawk imports
import Hawk.Controller
import Hawk.Model
import Hawk.View.Template.HtmlHelper as Html
import Hawk.View.TemplateView
import Hawk.Controller.Util.Monad
import Hawk.View.Template.DataType
import Hawk.Controller.Util.Read (maybeRead)

-- other stuff
import Data.Maybe (fromMaybe, fromJust)

showEditLink :: Integer -> [XmlTree]
showEditLink i = [Html.link ("/step/edit?id=" ++ show i) [Html.image "Edit Step" "/images/edit.png" []]]

$(viewDataType "Step" "_header")
$(viewDataType "Step" "_step")
$(viewDataType "Step" "show")
$(viewDataTypeWithPrefix "Edit" "Step" "edit")

controllers :: [Routing]
controllers =  [ ("show", userAuthorize stepShowAction >>= render (typedView "show" stepShowXhtml') )
               , ("edit", userAuthorize stepEditAction >>= render (typedView "edit" stepEditXhtml') )
               ]


stepShowAction :: StateController Step.Step
stepShowAction = do
  step' <- readParam "id" >>= liftMaybe findMaybe
  case step' of
    Nothing -> do
      setFlash "error" "the requested step could not be found"
      redirectToAction "diary" "index"
    Just s  -> Step.getPackage s >>= Package.getProject >>= projectAuthorize >> return s


stepHeader :: Step.Step -> Bool -> StateController Step_header
stepHeader step' isNew = do
  package <- Step.getPackage step'
  user    <- Step.getUser step'
  area    <- Package.getArea package
  project' <- Area.getProject area
  let path1 =  [ Html.textlink "/diary" (Project.name project'), Html.text " => "
               , Html.textlink ("/area/show?id=" ++ show (Area._id area)) (Area.description area), Html.text " => "
               , Html.textlink ("/package/show?id=" ++ show (Package._id package)) (Package.description package), Html.text " => "
               ]
  let path2 = if isNew then [Html.text "New"]
                       else [Html.textlink ("/step/show?id=" ++ show (Step._id step')) (Step.description step')]
  templatesHeader' <- templatesHeader
  return Step_header { createdAt = maybe "" show $ Step.createdAt step'
                     , createdBy = User.name user
                     , comment = fromMaybe "" $ Step.comment step'
                     , templatesPath = [TemplatesPath (path1 ++ path2)]
                     , templates_header = templatesHeader'}


stepShowXhtml' :: Step.Step -> StateController StepShow
stepShowXhtml' s = do
        stepHeader' <- stepHeader s False
        return StepShow {
                step_header = [stepHeader'],
                step_step = [stepStepXhtml s]
            }

stepStepXhtml :: Step.Step -> Step_step
stepStepXhtml s = Step_step {
                    step = [Html.textlink ("/step/show?id=" ++ show (Step._id s)) (Step.description s)],
                    stepWorkNow = Step.duration s,
                    stepCompleteness = ((Step.color s), fromIntegral $ Step.completion s),
                    stepEditlink = Step._id s
                    }

stepEditAction :: StateController (Step.Step, Bool)
stepEditAction = do
  (step', isNew) <- findOrCreate'
  Step.getPackage step' >>= Package.getProject >>= projectAuthorize
  method <- getRequestMethod
  case method of
    POST -> do
      (s, errs) <- getParams >>= updateAndValidate step' ""
      if null errs then do
        if isNew then do
          u <- readSessionValue "user_id"
          insert $ s { Step.userId = u }
          else update s
        setFlash "notice" "Your changes have been saved"
        redirectToAction "diary" "index"
        else do
          setErrors "step" errs
          return (s, isNew)
    _ -> return (step', isNew)


findOrCreate' :: StateController (Step.Step, Bool)
findOrCreate' = do
  (p, isNew) <- findOrCreate
  userId  <- getSessionValue "user_id"
  if isNew then do
    package <- readParam "package_id" >>= liftMaybe findMaybe
    case package of
      Nothing -> do
        setFlash "error" "the requested package could not be found"
        redirectToAction "diary" "index"
      Just a -> return (p { Step.packageId = Just $ Package._id a, Step.userId = userId >>= maybeRead }, isNew)
    else return (p { Step.userId = userId >>= maybeRead }, isNew)



stepShowXhtml :: Html.XmlTree -> Step.Step -> StateController [Html.XmlTree]
stepShowXhtml template s = do
  sh <- stepHeaderXhtml s False $ head $ subTemplate template "step.header"
  return $ bind template
    [ ("step.header" , sh)
    , ("step"        , [Html.textlink ("/step/show?id=" ++ show (Step._id s)) (Step.description s)])
    , ("stepWorkNow" , [Html.showtext $ Step.duration s])
    , ("stepCompleteness", [completenessTag (Step.color s) $ fromIntegral $ Step.completion s])
    , ("stepEditlink", [Html.link ("/step/edit?id=" ++ show (Step._id s)) [Html.image "Edit Step" "/images/edit.png" []]])
    ] []


stepEditXhtml :: Html.XmlTree -> (Step.Step, Bool) -> StateController [Html.XmlTree]
stepEditXhtml template (s, isNew) = do
  sh <- stepHeaderXhtml s isNew $ head $ subTemplate template "step.header"
  return $ bind template
    [ ("step.header", sh)
    , ("description", [Html.textfield "description" (Step.description s) []])
    , ("duration"   , [Html.textfield "duration"    (show $ Step.duration s) []])
    , ("completion" , [Html.textfield "completion"  (show $ Step.completion s) []])
    , ("comment"    , [Html.textarea  "comment"     (fromMaybe "" $ Step.comment s) []])
    , ("package_id" , [Html.hidden    "package_id"  (show $ Step.packageId s) [] | isNew])
    , ("id"         , [Html.hidden    "id"          (show $ Step._id s) [] | not isNew])
    ] []

stepEditXhtml' :: (Step.Step, Bool) -> StateController StepEdit
stepEditXhtml' (s, isNew) = do
  sh <- stepHeader s isNew
  return StepEdit {
    editStep_header = [sh],
    editDescription = [Html.textfield "description" (Step.description s) []],
    editDuration    = [Html.textfield "duration"    (show $ Step.duration s) []],
    editCompletion  = [Html.textfield "completion"  (show $ Step.completion s) []],
    editComment     = [Html.textarea  "comment"     (fromMaybe "" $ Step.comment s) []],
    editPackage_id  = [Html.hidden    "package_id"  (show $ fromJust $ Step.packageId s) [] | isNew],
    editId          = [Html.hidden    "id"          (show $ Step._id s) [] | not isNew]
    }


stepHeaderXhtml :: Step.Step -> Bool -> Html.XmlTree -> StateController [Html.XmlTree]
stepHeaderXhtml step' isNew template = do
  dh <- diaryHeaderXhtml $ head $ subTemplate template "diary.header"
  user    <- Step.getUser step'
  package <- Step.getPackage step'
  area    <- Package.getArea package
  project' <- Area.getProject area
  let path1 =  [ Html.textlink "/diary" (Project.name project'), Html.text " => "
               , Html.textlink ("/area/show?id=" ++ show (Area._id area)) (Area.description area), Html.text " => "
               , Html.textlink ("/package/show?id=" ++ show (Package._id package)) (Package.description package), Html.text " => "
               ]
  let path2 = if isNew then [Html.text "New"]
                       else [Html.textlink ("/step/show?id=" ++ show (Step._id step')) (Step.description step')]
  return $ bind template
    [ ("diary.header", dh)
    , ("createdAt"   , [Html.text $ show $ fromJust $ Step.createdAt step'])
    , ("createdBy"   , [Html.text $ User.name user])
    , ("comment"     , [Html.text $ fromMaybe "" $ Step.comment step'])
    , ("path"        , path1 ++ path2)
    ] []
