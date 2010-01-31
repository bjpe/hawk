-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Area
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (TODO use at least Criteria ....)
   Version     :  $Id: Area.hs 1001 2009-10-04 16:47:40Z inf6509 $

   The functions for a Area
-}
-- ----------------------------------------------------------------------------
module App.Model.Area
    ( module App.Model.Area.Type
    , module App.Model.Area.Functions
    , module App.Model.Area
    ) where

import App.Model.Area.Type
import App.Model.Area.Functions
import qualified App.Model.Package as Package
import qualified App.Model.User as User

import Hawk.Model

import Control.Monad (liftM, (>=>))

deleteCascading :: MonadDB m => Area -> m Bool
deleteCascading a = do
  getPackages a >>= mapM Package.deleteCascading
  delete a
  
-- Fertigstellungsgrad des Arbeitsgebiets
-- Die Summe der geleisteteten Arbeitsstunden in Relation zur Summe der pro 
-- Paket zu erwartenden Arbeitsaufw�nde
completeness :: MonadDB m => Area -> m Double
completeness a = do
  w <- workEstimated a
  if w <= 0.0
     then return 0.0
     else do
          n <- workNow a
          return $ 100.0 * n / w

-- minimaler Aufwand des Arbeitsgebiets
workMin :: MonadDB m => Area -> m Double
workMin = liftM (sum . map Package.workMin) . getPackages

-- maximaler Aufwand des Arbeitsgebiets
workMax :: MonadDB m => Area -> m Double
workMax = liftM (sum . map Package.workMax) . getPackages

-- bisheriger Aufwand des Arbeitsgebiets
workNow :: MonadDB m => Area -> m Double
workNow = getPackages >=> mapM Package.workNow >=> return . sum

-- gesch�tzter Aufwand des Arbeitsgebiets
workEstimated :: MonadDB m => Area -> m Double
workEstimated = getPackages >=> mapM Package.workEstimated >=> return . sum
  
-- Aufwand des Benutzers user f�r das Gebiet
usertime :: MonadDB m => User.User -> Area -> m Double
usertime u = getPackages >=> mapM (Package.usertime u) >=> return . sum

-- Statusfarbe des Arbeitsgebiets
color :: MonadDB m => Area -> m String
color a = do
    work <- workEstimated a
    help work [return 0.0, workMin a, workMax a] ["yellow", "green", "yellow", "red"]
{-do
  work <- workEstimated a
  if (work <= 0.0)
     then return "yellow"
     else do
          wMin <- workMin a
          if (work <= wMin)
             then return "green"
             else do
                  wMax <- workMax a
                  if (work <= wMax)
                     then return "yellow"
                     else return "red"
                     -}

help :: (Monad m, Ord a) => a -> [m a] -> [b] -> m b
help _ []     (x:_)  = return x
help w (v:vs) (x:xs) = do
    v' <- v
    if w <= v' then return x else help w vs xs
help _ _      []     = fail "not enough Strings"

