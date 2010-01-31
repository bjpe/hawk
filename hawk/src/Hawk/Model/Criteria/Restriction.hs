-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Restriction.hs 307 2009-05-06 10:29:12Z inf6254 $

   Restrictions for the criteria api
-}
-- ----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Hawk.Model.Criteria.Restriction
  (
  -- * Data types and constructors
    Restriction
  , CompareValue
  , col
  , val
  , sqlVal
  -- * Restriction combinators
  , andExpr
  , (.&&.)
  , orExpr
  , (.||.)
  , allExpr
  , anyExpr
  , notExpr
  -- * SimpleRestriction constructors
  , nullExpr
  , notNullExpr
  , eqExpr
  , (.==.)  
  , neExpr
  , (./=.)
  , gtExpr
  , (.>.)
  , ltExpr
  , (.<.)
  , geExpr
  , (.>=.)
  , leExpr
  , (.<=.)
  , likeExpr
  , notlikeExpr
  , betweenExpr
  , inExpr
  ) where

import Hawk.Model.Criteria.Types
import Data.List (intercalate)
import Data.Convertible (Convertible)

-- | A Restriction
data Restriction = Simple SimpleRestriction
                 | NotExpr Restriction
                 | LogicExpr LogicOp [Restriction]

-- | The Operators And and Or
data LogicOp = And | Or

-- * Restriction combinators
-- | True if both Restrictions are true
andExpr :: Restriction -> Restriction -> Restriction
andExpr lhs rhs = allExpr [lhs, rhs]

(.&&.) :: Restriction -> Restriction -> Restriction
(.&&.) = andExpr

-- | True if one Restrictions is true
orExpr :: Restriction -> Restriction -> Restriction
orExpr lhs rhs = anyExpr [lhs, rhs]

(.||.) :: Restriction -> Restriction -> Restriction
(.||.) = orExpr

-- | True if all Restrictions are true
allExpr :: [Restriction] -> Restriction
allExpr = LogicExpr And

-- | True if one Restrictions is true
anyExpr :: [Restriction] -> Restriction
anyExpr = LogicExpr Or

-- | The not Operator
notExpr :: Restriction -> Restriction
notExpr = NotExpr

-- | Simple Restriction
data SimpleRestriction = UnExpr UnOp CompareValue
                       | BinExpr BinOp CompareValue CompareValue
                       | Between CompareValue CompareValue CompareValue
                       | In CompareValue [SqlValue]

data CompareValue = Col ColumnName
                  | Val SqlValue

-- | Construct a CompareValue from a Value in a Column
col :: ColumnName -> CompareValue
col = Col

-- | Construct a CompareValue from a Value
val :: Convertible a SqlValue => a -> CompareValue
val = Val . toSql

-- | Construct a CompareValue from a SqlValue
sqlVal :: SqlValue -> CompareValue
sqlVal = Val

-- | unary Operations
data UnOp = IsNull
          | IsNotNull

-- | binary Operations
data BinOp = Equal
           | NotEqual
           | GreaterThan
           | LessThan
           | GreaterThanOrEqual
           | LessThanOrEqual
           | Like
           | NotLike

--  SimpleRestriction constructors
-- | true if a column have a NULL Value
nullExpr :: CompareValue -> Restriction
nullExpr = Simple . UnExpr IsNull

-- | true if a column have a not a NULL Value
notNullExpr :: CompareValue -> Restriction
notNullExpr = Simple . UnExpr IsNotNull

-- | Compare the Value of two Columns. True if equal.
eqExpr :: CompareValue -> CompareValue -> Restriction
eqExpr l = Simple . BinExpr Equal l

(.==.) :: CompareValue -> CompareValue -> Restriction
(.==.) = eqExpr

-- | Compare the Value of two Columns. True if not equal.
neExpr :: CompareValue -> CompareValue -> Restriction
neExpr l = Simple . BinExpr NotEqual l

(./=.) :: CompareValue -> CompareValue -> Restriction
(./=.) = neExpr

-- | Compare the Value of two Columns. True if first value is greater then the second.
gtExpr :: CompareValue -> CompareValue -> Restriction
gtExpr l = Simple . BinExpr GreaterThan l

(.>.) :: CompareValue -> CompareValue -> Restriction
(.>.) = gtExpr

-- | Compare the Value of two Columns. True if first value is less then the second.
ltExpr :: CompareValue -> CompareValue -> Restriction
ltExpr l = Simple . BinExpr LessThan l

(.<.) :: CompareValue -> CompareValue -> Restriction
(.<.) = ltExpr

-- | Compare the Value of two Columns. True if first value is greater or equal then the second.
geExpr :: CompareValue -> CompareValue -> Restriction
geExpr l = Simple . BinExpr GreaterThanOrEqual l

(.>=.) :: CompareValue -> CompareValue -> Restriction
(.>=.) = geExpr

-- | Compare the Value of two Columns. True if first value is less or equal then the second.
leExpr :: CompareValue -> CompareValue -> Restriction
leExpr l = Simple . BinExpr LessThanOrEqual l

(.<=.) :: CompareValue -> CompareValue -> Restriction
(.<=.) = leExpr

-- | Test, if a column is like a String
likeExpr :: CompareValue -> CompareValue -> Restriction
likeExpr l = Simple . BinExpr Like l

-- | Test, if a column is not like a String
notlikeExpr :: CompareValue -> CompareValue -> Restriction
notlikeExpr l = Simple . BinExpr NotLike l

-- | Test, if the first value is between the second and third
betweenExpr :: CompareValue -> CompareValue -> CompareValue -> Restriction
betweenExpr v l = Simple . Between v l

-- | Test, if the first row is in the list of values.
inExpr :: Convertible a SqlValue => CompareValue -> [a] -> Restriction
inExpr l = Simple . In l . map toSql

instance ShowSql LogicOp where
  showSql And = " AND "
  showSql Or  = " OR "

instance ShowSql UnOp where
  showSql IsNull    = " IS NULL "
  showSql IsNotNull = " IS NOT NULL "

instance ShowSql BinOp where
  showSql Equal              = " == "
  showSql NotEqual           = " <> "
  showSql GreaterThan        = " > "
  showSql LessThan           = " < "
  showSql GreaterThanOrEqual = " >= "
  showSql LessThanOrEqual    = " <= "
  showSql Like               = " LIKE "
  showSql NotLike            = " NOT LIKE "

instance ShowSql CompareValue where
  showSql (Col c) = c
  showSql (Val v) = showSql v

instance ShowSql SimpleRestriction where
  showSql (UnExpr op c)
    = showSql op ++ paren (showSql c)
  showSql (BinExpr op c1 c2)
    = showSql c1 ++ showSql op ++ showSql c2
  showSql (Between c1 c2 c3)
    = showSql c1 ++ " BETWEEEN " ++ showSql c2 ++ " AND " ++ showSql c3
  showSql (In c vs)  = showSql c ++ " IN " ++ paren placeHolder
    where placeHolder = intercalate ", " $ map showSql vs

instance ShowSql Restriction where
  showSql (Simple s)        = showSql s
  showSql (NotExpr c)       = " NOT " ++ paren (showSql c)
  showSql (LogicExpr op cs)
    = intercalate (showSql op) $ map (paren . showSql) cs

instance GetValues CompareValue where
  getValues (Col _) = []
  getValues (Val v) = [v]

instance GetValues SimpleRestriction where
  getValues (UnExpr _ c)       = getValues c
  getValues (BinExpr _ c1 c2)  = concatMap getValues [c1,c2]
  getValues (Between c1 c2 c3) = concatMap getValues [c1,c2,c3]
  getValues (In c vs)          = getValues c ++ vs

instance GetValues Restriction where
  getValues (Simple s)       = getValues s
  getValues (NotExpr c)      = getValues c
  getValues (LogicExpr _ cs) = concatMap getValues cs

paren :: String -> String
paren s = '(' : s ++ ")"
