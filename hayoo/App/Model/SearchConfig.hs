module App.Model.SearchConfig where

import Hawk.Model

data SearchConfig = SearchConfig
  { _uid          :: ForeignKey
  , useCase       :: Maybe Bool -- null means find as you type
  , useFuzzy      :: Bool
  , allowPackages :: Bool
  , optimizeQuery :: Bool
  , wordLimit     :: Int
  } deriving (Eq, Read, Show)

instance Persistent SearchConfig where
  persistentType _ = "SearchConfig"
  fromSqlList (l0:l1:l2:l3:l4:l5[])
    = SearchConfig (fromSql l0) (fromSql l1) (fromSql l2)
      (fromSql l3) (fromSql l4) (fromSql l5)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_uid"          , toSql $ _uid          x)
              , ("useCase"       , toSql $ useCase       x) 
              , ("useFuzzy"      , toSql $ useFuzzy      x)
              , ("allowPackages" , toSql $ allowPackages x)
              , ("optimizeQuery" , toSql $ optimizeQuery x)
              , ("wordLimit"     , toSql $ wordLimit     x)
              ]
  tableName = const "searchConfig"

-- primary key functionality
instance WithPrimaryKey SearchConfig where
  primaryKey = _uid
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_uid = pk}

data HasConfig = HasConfig

instance ForeignKeyRelationship HasConfig where
  type Referencing = User
  type Referenced  = SearchConfig
  relationshipName = "hasConfig"
  foreignKey = _uid
  fkColumn = "_uid"
  setForeignKey fk c = c {_uid = fk}

BelongsTo HasConfig -- searchConfig belongs to an user
HasOne HasConfig    -- each existing searchConfig has only one user

instance Model SearchConfig where
  new = return $ SearchConfig 0 Nothing False True True 0
  
instance Updateable SearchConfig where
  updater sc s = do
    u  <- updater (_uid          sc) $ subParam s "_uid"
    uc <- updater (useCase       sc) $ subParam s "useCase"
    uf <- updater (useFuzzy      sc) $ subParam s "useFuzzy"
    a  <- updater (allowPackages sc) $ subParam s "allowPackages"
    o  <- updater (optimizeQuery sc) $ subParam s "optimizeQuery"
    w  <- updater (wordLimit     sc) $ subParam s "wordLimit"
    return $ sc 
      { _uid = u
      , useCase = uc
      , useFuzzy = uf
      , allowPackages = a
      , optimizeQuery = o
      , wordLimit = w }

instance Validatable SearchConfig where
  validator sc = do
    validateNotNull "_uid"          $ _uid sc
    validateNotNull "useFuzzy"      $ useFuzzy sc
    validateNotNull "allowPackages" $ allowPackages sc
    validateNotNull "optimizeQuery" $ optimizeQuery sc
    validateNotNull "wordLimit"     $ wordLimit sc
    validateUniqueness [("_uid", toSql $ _uid fc)] _uid "_uid" fc
    return ()

