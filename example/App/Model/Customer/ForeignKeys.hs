{-# LANGUAGE TypeFamilies #-}
module App.Model.Customer.ForeignKeys where

import App.Model.Customer.Type (Customer(..))
import App.Model.Category.Type (Category)
import Hawk.Model

data Customer2Category = Customer2Category

instance ForeignKeyRelationship Customer2Category where
  type Referencing Customer2Category = Customer
  type Referenced  Customer2Category = Category
  relationshipName _ = "customer2category"
  fkColumn _ = "category_id"
  foreignKey _ = categoryId
  setForeignKey _ package key = package {categoryId = key}

instance BelongsTo Customer2Category
instance HasMany   Customer2Category