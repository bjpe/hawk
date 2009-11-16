-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Base.Area.ForeignKeys
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Relations for a Area
-}
-- ----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module App.Model.Area.ForeignKeys where

import App.Model.Area.Type (Area(..))
import App.Model.Project.Type (Project)

import Hawk.Model

data Area2Project = Area2Project

instance ForeignKeyRelationship Area2Project where
  type Referencing Area2Project = Area
  type Referenced  Area2Project = Project

  relationshipName _ = "area2project"

  -- getFKColumn   :: relationshipName -> ColumnName
  fkColumn _ = "project_id"
  -- getForeignKey :: relationshipName -> self -> Maybe PrimaryKey
  foreignKey _ = projectId
  -- setForeignKey :: relationshipName -> self -> Maybe PrimaryKey -> self
  setForeignKey _ area key = area {projectId = key}

instance BelongsTo Area2Project
instance HasMany Area2Project
