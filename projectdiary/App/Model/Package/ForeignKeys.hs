-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Relations for a Package
-}
-- ----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module App.Model.Package.ForeignKeys where

import App.Model.Package.Type (Package(..))
import App.Model.Area.Type (Area)

import Hawk.Model

data Package2Area = Package2Area

instance ForeignKeyRelationship Package2Area where
  type Referencing Package2Area = Package
  type Referenced  Package2Area = Area
  relationshipName _ = "package2area"
  -- getFKColumn   :: relationshipName -> ColumnName
  fkColumn _ = "area_id"
  -- getForeignKey :: relationshipName -> self -> Maybe PrimaryKey
  foreignKey _ = areaId
  -- setForeignKey :: relationshipName -> self -> Maybe PrimaryKey -> self
  setForeignKey _ package key = package {areaId = key}

instance BelongsTo Package2Area
instance HasMany Package2Area
