module App.Model.FuzzyConfig where

import Hawk.Model

data FuzzyConfig = FuzzyConfig
  { _uid         :: ForeignKey
  , replace      :: Bool
  , swapChars    :: Bool
  , replacements :: String
  , maxFuzzyness :: Float
  } deriving (Eq, Read, Show)

instance Persistent FuzzyConfig where
  persistentType _ = "FuzzyConfig"
  fromSqlList (l0:l1:l2:l3:l4:[])
    = FuzzyConfig (fromSql l0) (fromSql l1) (fromSql l2)
      (fromSql l3) (fromSql l4)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_uid"         , toSql $ _uid         x)
              , ("replace"      , toSql $ replace      x) 
              , ("swapChars"    , toSql $ swapChars    x)
              , ("replacements" , toSql $ replacements x)
              , ("maxFuzzyness" , toSql $ maxFuzzyness x)
              ]
  tableName = const "fuzzyConfig"

-- primary key functionality
instance WithPrimaryKey FuzzyConfig where
  primaryKey = _uid
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_uid = pk}

data HasFuzzy = HasFuzzy

instance ForeignKeyRelationship HasFuzzy where
  type Referencing = SearchConfig
  type Referenced  = FuzzyConfig
  relationshipName = "hasFuzzy"
  foreignKey = _uid
  fkColumn = "_uid"
  setForeignKey fk c = c {_uid = fk}

BelongsTo HasFuzzy -- fuzzyConfig belongs to a searchConfig
HasOne HasFuzzy    -- each existing fuzzyConfig has only one searchConfig

instance Model FuzzyConfig where
  new = return $ FuzzyConfig 0 False False [] 1.0
  
instance Updateable FuzzyConfig where
  updater fc s = do
    u  <- updater (_uid         fc) $ subParam s "_uid"
    r  <- updater (replace      fc) $ subParam s "replace"
    sc <- updater (swapChars    fc) $ subParam s "swapChars"
    re <- updater (replacements fc) $ subParam s "replacements"
    f  <- updater (maxFuzzyness fc) $ subParam s "maxFuzzyness"
    return $ fc 
      { _uid = u
      , replace = r
      , swapChars = sc
      , replacements = re
      , maxFuzzyness = f }

instance Validatable FuzzyConfig where
  validator fc = do
    validateNotNull "_uid"         $ _uid         fc
    validateNotNull "replace"      $ replace      fc
    validateNotNull "swapChars"    $ swapChars    fc
    validateNotNull "replacements" $ replacements fc
    validateNotNull "maxFuzzyness" $ maxFuzzyness fc
    validateUniqueness [("_uid", toSql $ _uid fc)] _uid "_uid" fc
    return ()

