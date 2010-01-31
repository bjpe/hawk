-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Package
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (TODO use at least Criteria ....)
   Version     :  $Id: Package.hs 1001 2009-10-04 16:47:40Z inf6509 $

   The functions for a Package
-}
-- ----------------------------------------------------------------------------
module App.Model.Package
    ( module App.Model.Package.Type
    , module App.Model.Package.Functions
    , module App.Model.Package
    ) where

import App.Model.Package.Type
import App.Model.Package.Functions
import qualified App.Model.Area.Functions as Area
import qualified App.Model.User as User
import qualified App.Model.Project.Type as Project
import qualified App.Model.Step as Step

import Hawk.Model

import Control.Monad (liftM, (>=>))
import Data.List (foldl')

deleteCascading :: MonadDB m => Package -> m Bool
deleteCascading p = do
  getSteps p >>= mapM delete
  delete p

-- Fertigstellungsgrades des Arbeitspakets
completeness :: MonadDB m => Package -> m Int
completeness = liftM (mean . map Step.completeness) . getSteps

mean :: [Int] -> Int
mean [] = 0
mean xs = s `div` n
  where (s,n) = foldl' (\(fs,fn) x -> (fs+x,fn+1)) (0,0) xs


-- Projekt des Arbeitspakets
getProject :: MonadDB m => Package -> m Project.Project
getProject = getArea >=> Area.getProject

-- bisheriger Aufwand des Arbeitspakets
workNow :: MonadDB m => Package -> m Double
workNow = liftM (sum . map Step.duration) . getSteps

-- Aufwand des Benutzers user f�r das Arbeitspaket
usertime :: MonadDB m => User.User -> Package -> m Double
usertime user = liftM (sum . map (Step.usertime user)) . getSteps

-- gesch�tzter Aufwand des Arbeitspakets
-- entsprechend des bisherigen Fortschritts angenommener Arbeitsaufwand
workEstimated :: MonadDB m => Package -> m Double
workEstimated p = do
  c <- completeness p
  if c <= 0
     then return 0.0
     else do
          n <- workNow p
          return $ 100.0 * n / fromIntegral c

-- Statusfarbe des Arbeitspakets
color :: MonadDB m => Package -> m String
color p = do
  work <- workEstimated p
  if work <= 0.0
     then return "yellow"
     else if work <= workMin p
             then return "green"
             else if work <= workMax p
                     then return "yellow"
                     else return "red"
