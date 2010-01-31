module App.Model.PackageConfig where

import Hawk.Model

data PackageConfig = PackageConfig
  { _uid     :: ForeignKey
  , _pid     :: ForeignKey
  , allowed  :: Bool
  , ranking  :: Int
  } deriving (Eq, Read, Show)

instance Persistent PackageConfig where
  persistentType _ = "PackageConfig"
  fromSqlList (l0:l1:l2:l3:[])
    = PackageConfig (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_uid"    , toSql $ _uid    x)
              , ("_pid"    , toSql $ _pid    x) 
              , ("allowed" , toSql $ allowed x)
              , ("ranking" , toSql $ ranking x)
              ]
  tableName = const "packageConfig"

-- primary key functionality
{-instance WithPrimaryKey PackageConfig where
  primaryKey = _uid
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_uid = pk}
-}
data AllowedPackages = AllowedPackages

instance ForeignKeyRelationship AllowedPackages where
  type Referencing = SearchConfig
  type Referenced  = PackageConfig
  relationshipName = "allowedPackages"
  foreignKey = _uid
  fkColumn = "_uid"
  setForeignKey fk c = c {_uid = fk}

BelongsTo AllowedPackages -- packageConfig belongs to a searchConfig
HasMany AllowedPackages   -- each existing searchConfig has 0..n packageConfig

data HasConfig = HasConfig

instance ForeignKeyRelationship HasConfig where
  type Referencing = Packages
  type Referenced  = PackageConfig
  relationshipName = "hasConfig"
  foreignKey = _pid
  fkColumn = "_pid"
  setForeignKey fk c = c {_pid = fk}

BelongsTo HasConfig -- packageConfig belongs to a package
HasMany HasConfig   -- each existing package has 0..n packageConfig

instance Model PackageConfig where
  new = return $ PackageConfig 0 0 False 0 
  
instance Updateable PackageConfig where
  updater pc s = do
    u <- updater (_uid    pc) $ subParam s "_uid"
    p <- updater (_pid    pc) $ subParam s "_pid"
    a <- updater (allowed pc) $ subParam s "allowed"
    r <- updater (ranking pc) $ subParam s "ranking"
    return $ pc { _uid = u, _pid = p, allowed = a, ranking = r }

instance Validatable PackageConfig where
  validator pc = do
    validateNotNull "_uid"    $ _uid    pc
    validateNotNull "_pid"    $ _pid    pc
    validateNotNull "allowed" $ allowed pc
    validateNotNull "ranking" $ ranking pc
    return ()

