module App.Model.User 
  ( User (..)
  , toList
  ) where

import Hawk.Model

data User = User
  { _uid           :: PrimaryKey
  , username       :: String
  , password       :: String
  , email          :: Maybe String
  , caseSensitive  :: Maybe Bool -- null means find as you type
  , optimizeQuery  :: Bool
  , wordLimit      :: Int
  , replace        :: Bool
  , swapChars      :: Bool
  , replacements   :: Maybe String
  , maxFuzzy       :: Double
  , modules        :: Maybe String
  , packages       :: Maybe String
  } deriving (Eq, Read, Show)

instance Persistent User where
  persistentType _ = "User"
  fromSqlList (l0:l1:l2:l3:l4:l5:l6:l7:l8:l9:l10:l11:l12:[])
    = User (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3)
           (fromSql l4) (fromSql l5) (fromSql l6) (fromSql l7)
           (fromSql l8) (fromSql l9) (fromSql l10) (fromSql l11)
           (fromSql l12)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_uid"          , toSql $ _uid           x)
              , ("username"      , toSql $ username       x) 
              , ("password"      , toSql $ password       x)
              , ("email"         , toSql $ email          x)
              , ("caseSensitive" , toSql $ caseSensitive  x) 
              , ("optimizeQuery" , toSql $ optimizeQuery  x)
              , ("wordLimit"     , toSql $ wordLimit      x)
              , ("replace"       , toSql $ replace        x)
              , ("swapChars"     , toSql $ swapChars      x)
              , ("replacements"  , toSql $ replacements   x)
              , ("maxFuzzy"      , toSql $ maxFuzzy       x)
              , ("modules"       , toSql $ modules        x)
              , ("packages"      , toSql $ packages       x)
              ]
  tableName = const "user"

-- primary key functionality
instance WithPrimaryKey User where
  primaryKey = _uid
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_uid = pk}

instance Model User where
  new = return $ User 0 "" "" Nothing Nothing True 0 False False Nothing 0.0 Nothing Nothing
  
instance Updateable User where
  updater u s = do
    user <- updater (username       u) $ subParam s "username"
    pass <- updater (password       u) $ subParam s "password"
    mail <- updater (email          u) $ subParam s "email"
    uc   <- updater (caseSensitive  u) $ subParam s "caseSensitive"
    o    <- updater (optimizeQuery  u) $ subParam s "optimizeQuery"
    w    <- updater (wordLimit      u) $ subParam s "wordLimit"
    fr   <- updater (replace        u) $ subParam s "replace"
    fs   <- updater (swapChars      u) $ subParam s "swapChars"
    fp   <- updater (replacements   u) $ subParam s "replacements"
    fm   <- updater (maxFuzzy       u) $ subParam s "maxFuzzy"
    mdl  <- updater (modules        u) $ subParam s "modules"
    pkg  <- updater (packages       u) $ subParam s "packages"
    return $ u 
      { username = user
      , password = pass
      , email = mail
      , caseSensitive = uc
      , optimizeQuery = o
      , wordLimit = w
      , replace = fr
      , swapChars = fs
      , replacements = fp
      , maxFuzzy = fm
      , modules = mdl
      , packages = pkg }

instance Validatable User where
  validator u = do
    validateNotNull    "username"       $ username u
    validateNotNull    "password"       $ password u
    validateUniqueness [("username", toSql $ username u)] username "username" u
    return ()

toList :: User -> [(String, String)]
toList u = map (\(x,y) -> (x, show y)) $ toSqlAL u

