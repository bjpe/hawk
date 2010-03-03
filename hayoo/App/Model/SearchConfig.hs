{-# LANGUAGE TypeFamilies #-}
module App.Model.SearchConfig where

import Hawk.Model
import App.Model.User (User)

data SearchConfig = SearchConfig
  { _cid           :: PrimaryKey
  , _uid           :: Maybe PrimaryKey
  , useCase        :: Maybe Bool -- null means find as you type
  , optimizeQuery  :: Bool
  , wordLimit      :: Int
  , f_replace      :: Bool
  , f_swapChars    :: Bool
  , f_replacements :: Maybe String
  , f_max          :: Float
  , modules        :: Maybe String
  , packages       :: Maybe String
  } deriving (Eq, Read, Show)

instance Persistent SearchConfig where
  persistentType _ = "SearchConfig"
  fromSqlList (l0:l1:l2:l3:l4:l5:l6:l7:l8:l9:l10:[])
    = SearchConfig (fromSql l0) (fromSql l1) (fromSql l2)
      (fromSql l3) (fromSql l4) (fromSql l5) (fromSql l6)
      (fromSql l7) (fromSql l8) (fromSql l9) (fromSql l10)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_cid"          , toSql $ _cid           x)
              , ("_uid"          , toSql $ _uid           x)
              , ("useCase"       , toSql $ useCase        x) 
              , ("optimizeQuery" , toSql $ optimizeQuery  x)
              , ("wordLimit"     , toSql $ wordLimit      x)
              , ("f_replace"     , toSql $ f_replace      x)
              , ("f_swapChars"   , toSql $ f_swapChars    x)
              , ("f_replacements", toSql $ f_replacements x)
              , ("f_max"         , toSql $ f_max          x)
              , ("modules"       , toSql $ modules        x)
              , ("packages"      , toSql $ packages       x)
              ]
  tableName = const "searchConfig"

-- primary key functionality
instance WithPrimaryKey SearchConfig where
  primaryKey = _cid
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_cid = pk}

data HasConfig = HasConfig

instance ForeignKeyRelationship HasConfig where
  type Referencing HasConfig = User
  type Referenced HasConfig  = SearchConfig
  relationshipName _ = "hasConfig"
  foreignKey _ = _uid
  fkColumn = "_uid"
  setForeignKey _ fk c = c {_uid = fk}

BelongsTo HasConfig -- searchConfig belongs to an user
HasOne HasConfig    -- each existing searchConfig has only one user

instance Model SearchConfig where
  new = return $ SearchConfig 0 0 Nothing True 0 False False Nothing 0.0 Nothing Nothing
  
instance Updateable SearchConfig where
  updater sc s = do
    c   <- updater (_cid           sc) $ subParam s "_cid"
    u   <- updater (_uid           sc) $ subParam s "_uid"
    uc  <- updater (useCase        sc) $ subParam s "useCase"
    o   <- updater (optimizeQuery  sc) $ subParam s "optimizeQuery"
    w   <- updater (wordLimit      sc) $ subParam s "wordLimit"
    fr  <- updater (f_replace      sc) $ subParam s "f_replace"
    fs  <- updater (f_swapChars    sc) $ subParam s "f_swapChars"
    fp  <- updater (f_replacements sc) $ subParam s "f_replacements"
    fm  <- updater (f_max          sc) $ subParam s "f_max"
    mdl <- updater (modules        sc) $ subParam s "modules"
    pkg <- updater (packages       sc) $ subParam s "packages"
    return $ sc 
      { _cid = c
      , _uid = u
      , useCase = uc
      , optimizeQuery = o
      , wordLimit = w
      , f_replace = fr
      , f_swapChars = fs
      , f_replacements = fp
      , f_max = fm
      , modules = mdl
      , packages = pkg }

instance Validatable SearchConfig where
  validator sc = do
    validateNotNull "_cid"           $ _cid sc
    validateNotNull "optimizeQuery"  $ optimizeQuery sc
    validateNotNull "wordLimit"      $ wordLimit sc
    validateNotNull "f_replace"      $ f_replace sc
    validateNotNull "f_swapChars"    $ f_swapChars sc
    validateNotNull "f_max"          $ f_max sc
    validateUniqueness [("_cid", toSql $ _cid sc)] _cid "_cid" sc
    return ()
