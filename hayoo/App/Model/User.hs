module App.Model.User 
  ( User (..)
  ) where

import Hawk.Model

-- User table
-- - username : String
-- - password : String
-- - e-mail : String
-- - _id : int
data User = User
  { _uid           :: PrimaryKey
  , username       :: String
  , password       :: String
  , email          :: Maybe String
  , useCase        :: Maybe Bool -- null means find as you type
  , optimizeQuery  :: Bool
  , wordLimit      :: Int
  , f_replace      :: Bool
  , f_swapChars    :: Bool
  , f_replacements :: Maybe String
  , f_max          :: Double
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
    uc   <- updater (useCase        u) $ toBool $ subParam s "useCase"
    o    <- updater (optimizeQuery  u) $ toBool $ subParam s "optimizeQuery"
    w    <- updater (wordLimit      u) $ subParam s "wordLimit"
    fr   <- updater (f_replace      u) $ toBool $ subParam s "replace"
    fs   <- updater (f_swapChars    u) $ toBool $ subParam s "swapChars"
    fp   <- updater (f_replacements u) $ subParam s "replacements"
    fm   <- updater (f_max          u) $ subParam s "maxFuzzy"
    mdl  <- updater (modules        u) $ subParam s "modules"
    pkg  <- updater (packages       u) $ subParam s "packages"
    return $ u 
      { username = user
      , password = pass
      , email = mail
      , useCase = uc
      , optimizeQuery = o
      , wordLimit = w
      , f_replace = fr
      , f_swapChars = fs
      , f_replacements = fp
      , f_max = fm
      , modules = mdl
      , packages = pkg }


instance Validatable User where
  validator u = do
    validateNotNull    "username"       $ username u
    validateNotNull    "password"       $ password u
    validateUniqueness [("username", toSql $ username u)] username "username" u
    return ()

toBool :: String -> String
toBool "true" = "True"
toBool "on" = "True"
toBool _ = "False"

