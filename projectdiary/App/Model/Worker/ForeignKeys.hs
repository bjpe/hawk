-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Base.Worker.ForeignKeys
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Relations for a Worker
-}
-- ----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module App.Model.Worker.ForeignKeys where

import App.Model.Worker.Type (Worker(..))
import App.Model.Project.Type (Project)
import App.Model.User.Type (User)

import Hawk.Model

data Worker2Project = Worker2Project

instance ForeignKeyRelationship Worker2Project where
  type Referencing Worker2Project = Worker
  type Referenced  Worker2Project = Project
  relationshipName _ = "worker2project"
  -- getFKColumn   :: relationshipName -> ColumnName
  fkColumn _ = "project_id"
  -- getForeignKey :: relationshipName -> self -> Maybe PrimaryKey
  foreignKey _ = projectId
  -- setForeignKey :: relationshipName -> self -> Maybe PrimaryKey -> self
  setForeignKey _ x key = x {projectId = key}

instance BelongsTo Worker2Project
instance HasMany Worker2Project 

data Worker2User = Worker2User

instance ForeignKeyRelationship Worker2User where
  type Referencing Worker2User = Worker
  type Referenced  Worker2User = User
  relationshipName _ = "worker2user"
  -- getFKColumn   :: relationshipName -> ColumnName
  fkColumn _ = "user_id"
  -- getForeignKey :: relationshipName -> self -> Maybe PrimaryKey
  foreignKey _ = userId
  -- setForeignKey :: relationshipName -> self -> Maybe PrimaryKey -> self
  setForeignKey _ x key = x {userId = key}

instance BelongsTo Worker2User
instance HasMany Worker2User
