-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Project
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (TODO use at least Criteria ....)
   Version     :  $Id: Project.hs 1001 2009-10-04 16:47:40Z inf6509 $

   The functions for a Project
-}
-- ----------------------------------------------------------------------------
module App.Model.Project
    ( module App.Model.Project.Type
    , module App.Model.Project.Functions
    , module App.Model.Project 
    ) where

import App.Model.Project.Type
import App.Model.Project.Functions
import qualified App.Model.Area as Area
import qualified App.Model.User as User
import qualified App.Model.Worker as Worker

import Hawk.Model

import Control.Monad (liftM, (>=>))

-- Projektname als String (wird zusammengesetzt)
name :: Project -> String
name p = prefix p ++ " – " ++ description p

deleteCascading :: MonadDB m => Project -> m Bool
deleteCascading p = do
  getAreas p >>= mapM Area.deleteCascading
  getWorkers p >>= mapM delete
  delete p
  
-- Fertigstellungsgrades des Projekts
-- Die Summe der geleisteteten Arbeitsstunden in Relation zur Summe der 
-- pro Gebiet zu erwartenden Arbeitsaufw�nde
completeness :: MonadDB m => Project -> m Double
completeness p = do
  w <- workEstimated p
  if w <= 0.0
     then return 0.0
     else do
          n <- workNow p
          return $ 100.0 * n / w


-- minimaler Aufwand des Projekts
workMin :: MonadDB m => Project -> m Double
workMin = getAreas >=> liftM sum . mapM Area.workMin

-- maximaler Aufwand des Projekts
workMax :: MonadDB m => Project -> m Double
workMax = getAreas >=> liftM sum . mapM Area.workMax

-- bisheriger Aufwand des Projekts
workNow :: MonadDB m => Project -> m Double
workNow = getAreas >=> liftM sum . mapM Area.workNow

-- gesch�tzter Aufwand des Projekts
workEstimated :: MonadDB m => Project -> m Double
workEstimated = getAreas >=> liftM sum . mapM Area.workEstimated
 
-- Aufwand des Benutzers user f�r das Projekt
usertime :: MonadDB m => User.User -> Project -> m Double
usertime user = getAreas >=> liftM sum . mapM (Area.usertime user)

-- Statusfarbe des Projekts
color :: MonadDB m => Project -> m String
color p = do
  work <- workEstimated p
  if work <= 0.0
     then return "yellow"
     else do
          wMin <- workMin p
          if work <= wMin
             then return "green"
             else do
                  wMax <- workMax p
                  if work <= wMax
                     then return "yellow"
                     else return "red"

workingUsers :: MonadDB m => Project -> m [User.User]
workingUsers = getWorkers >=> mapM Worker.getUser
