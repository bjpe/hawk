-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Bj�rn Peem�ller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (TODO use at least Criteria ....)
   Version     :  $Id: Step.hs 808 2009-09-04 13:29:00Z inf6254 $

   The functions for a Step
-}
-- ----------------------------------------------------------------------------
module App.Model.Step
    ( module App.Model.Step.Type
    , module App.Model.Step.Functions
    , module App.Model.Step
    ) where

import App.Model.Step.Type
import App.Model.Step.Functions
import App.Model.User as User

completeness :: Step -> Int
completeness = completion

-- Gibt die Arbeitszeit des Schritts zur�ck, falls der Mitarbeiter der angegebene ist.
usertime :: User -> Step -> Double
usertime u s 
  | userId s == 
    (Just $ User._id u) = duration s
  | otherwise           = 0

-- Statusfarbe des Schritts
color :: Step -> String
color = const "blue"
