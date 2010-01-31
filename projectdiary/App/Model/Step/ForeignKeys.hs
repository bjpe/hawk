-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Base.Step.ForeignKeys
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Relations for a Step
-}
-- ----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module App.Model.Step.ForeignKeys where

import App.Model.Step.Type (Step(..))
import App.Model.Package.Type (Package)
import App.Model.User.Type (User)

import Hawk.Model

data Step2Package = Step2Package

instance ForeignKeyRelationship Step2Package where
  type Referencing Step2Package = Step
  type Referenced  Step2Package = Package
  relationshipName _ = "step2package"
  -- getFKColumn   :: relationshipName -> ColumnName
  fkColumn _ = "package_id"
  -- getForeignKey :: relationshipName -> self -> Maybe PrimaryKey
  foreignKey _ = packageId
  -- setForeignKey :: relationshipName -> self -> Maybe PrimaryKey -> self
  setForeignKey _ step key = step {packageId = key}

instance BelongsTo Step2Package
instance HasMany Step2Package

data Step2User = Step2User

instance ForeignKeyRelationship Step2User where
  type Referencing Step2User = Step
  type Referenced  Step2User = User
  relationshipName _ = "step2user"
  -- getFKColumn   :: relationshipName -> ColumnName
  fkColumn _ = "user_id"
  -- getForeignKey :: relationshipName -> self -> Maybe PrimaryKey
  foreignKey _ = userId
  -- setForeignKey :: relationshipName -> self -> Maybe PrimaryKey -> self
  setForeignKey _ step key = step {userId = key}

instance BelongsTo Step2User
instance HasMany Step2User
