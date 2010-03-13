module App.Model.Category.Type (Category (..)) where

import Hawk.Model

data Category = Category
  { _id      :: PrimaryKey
  , name     :: String
  , discount :: Int
  } deriving (Eq, Read, Show)
    
instance Persistent Category where
  persistentType _ = "Category"
  fromSqlList (l0:l1:l2:[]) = Category (fromSql l0) (fromSql l1) (fromSql l2)
  fromSqlList _             = error "wrong list length"
  toSqlAL x = [ ("_id"     , toSql $ _id      x)
              , ("name"    , toSql $ name     x) 
              , ("discount", toSql $ discount x)
              ]
  tableName = const "categories"

instance WithPrimaryKey Category where
  primaryKey         = _id
  pkColumn           = head . tableColumns
  setPrimaryKey pk p = p {_id = pk}

instance Model Category where
  new = return $ Category 0 "" 0

instance Validatable Category where
  validator c = do
    validateNotNull "name"     $ name     c
    validateUniqueness [] name "name"     c
    return ()

instance Updateable Category where
  updater c _name = do
    name' <- updater (name     c) $ subParam _name "name"
    disc' <- updater (discount c) $ subParam _name "discount"
    return $ c { name = name', discount = disc' }
