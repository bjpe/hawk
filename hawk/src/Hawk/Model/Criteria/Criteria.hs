-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Model.Sql
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Criteria.hs 291 2009-05-01 13:01:55Z inf6509 $

   Function for working with the whole Criteria data
-}
-- ----------------------------------------------------------------------------
module Hawk.Model.Criteria.Criteria
    (
    -- * Criteria
    Criteria (..)
    , setRestriction
    , newCriteria
    , restrictionCriteria
    , addExtra
    , addLimit
    , setOrder
    -- * Query
    , Query (..)
    , newSelect
    , setOption
    , setCriteria
    , setProjection
    , setTables
    , setGrouping
    , setHaving
    , modifyCriteria
    -- * Manipulation
    , Manipulation (..)
    , newDelete
    , newUpdate
    , newInsert
    , setManipulationCriteria
    , modifyManipulationCriteria
    , setTable
    , setSet
    , setValues
    ) where

import Hawk.Model.Criteria.Types
import Hawk.Model.Criteria.Projection
import Hawk.Model.Criteria.Restriction
import Hawk.Model.Criteria.Order
import Data.List (intercalate)

data Criteria = Criteria
    { restriction :: Maybe Restriction -- ^ WHERE
    , ordering    :: [Order]           -- ^ ORDER BY
    , extra       :: [String]          -- ^ LIMIT, OFFSET, ...
    }

data Query = Select
    { option     :: Maybe String      -- ^ ALL, DISTINCT, ...
    , projection :: [Projection]      -- ^ SELECT
    , tables     :: [TableName]       -- ^ FROM
    , grouping   :: [ColumnName]      -- ^ GROUP BY
    , having     :: Maybe Restriction -- ^ HAVING
    , criteria   :: Criteria          -- ^ WHERE, ORDER BY and LIMIT, OFFSET, ...
    }

data Manipulation =
      Delete
        { table    :: TableName
        , manipulationCriteria :: Criteria
        }
    | Update
        { table    :: TableName
        , set      :: [(ColumnName, SqlValue)]
        , manipulationCriteria :: Criteria
        }
    | Insert
        { table    :: TableName
        , values   :: [(ColumnName, SqlValue)]
        }

instance ShowSql Criteria where
    showSql c = whereClause (restriction c)
            ++ orderClause (ordering c)
            ++ extraClause (extra c)
        where
        whereClause r     = case r of
                          Nothing -> []
                          Just rs -> " WHERE " ++ showSql rs
        orderClause o     = case o of
                          [] -> ""
                          os -> " ORDER BY " ++ intercalate ", " (map showSql os)
        extraClause e     = ' ' : intercalate ", " e

instance ShowSql Query where
  showSql c =  optionsClause (option                 c)
            ++ selectClause  (projection             c)
            ++ fromClause    (tables                 c)
            ++ whereClause   (restriction $ criteria c)
            ++ groupClause   (grouping               c)
            ++ orderClause   (ordering    $ criteria c)
            ++ havingClause  (having                 c)
            ++ extraClause   (extra       $ criteria c)
      where
      optionsClause opt = case opt of
                          Nothing -> "SELECT"
                          Just o  -> "SELECT " ++ o
      selectClause proj = case proj of
                          [] -> " *"
                          p  -> ' ' : intercalate ", " (map showSql p)

      fromClause tbls   = " FROM " ++ intercalate ", " tbls
      whereClause r     = case r of
                          Nothing -> []
                          Just rs -> " WHERE " ++ showSql rs
      groupClause g     = case g of
                          [] -> ""
                          gs -> " GROUP BY " ++ intercalate ", " gs
      orderClause o     = case o of
                          [] -> ""
                          os -> " ORDER BY " ++ intercalate ", " (map showSql os)
      havingClause h    = case h of
                          Nothing -> []
                          Just hs -> " HAVING " ++ showSql hs
      extraClause e     = ' ' : intercalate ", " e

instance ShowSql Manipulation where
  showSql (Delete t   c) = showDelete t   c
  showSql (Update t s c) = showUpdate t s c
  showSql (Insert t s  ) = showInsert t s

showDelete :: TableName -> Criteria -> String
showDelete tbl cri =
    deleteClause ++ showSql cri
    where
        deleteClause = "DELETE FROM " ++ tbl

showUpdate :: TableName -> [(ColumnName, SqlValue)] -> Criteria -> String
showUpdate tbl s cri =
    updateClause ++ columnExpr ++ showSql cri
    where
        updateClause = "UPDATE " ++ tbl
        columnExpr   = " SET " ++ intercalate ", " (map (\(cn,_) -> cn ++ " = ? ") s)

showInsert :: TableName -> [(ColumnName, SqlValue)] -> String
showInsert tbl s =
    insertClause ++ columns ++ value
    where
        insertClause = "INSERT INTO " ++ tbl
        columns      = " (" ++ intercalate ", " (map fst s) ++ ")"
        value        = " VALUES (" ++ intercalate ", " (map (const "?") s) ++ ")"

instance GetValues Criteria where
    getValues = maybe [] getValues . restriction

instance GetValues Query where
    getValues (Select _ _ _ _ h c) = getValues c ++ maybe [] getValues h

instance GetValues Manipulation where
    getValues (Delete _ c) = getValues c
    getValues (Update _ s c) = map snd s ++ getValues c
    getValues (Insert _ s) = map snd s

-- * Criteria functions

-- | Create a empty Criteria
newCriteria :: Criteria
newCriteria = Criteria Nothing [] []

restrictionCriteria :: Restriction -> Criteria
restrictionCriteria r = setRestriction r newCriteria

-- | Set the Restriction of a Criteria (sql where clause)
setRestriction :: Restriction -> Criteria -> Criteria
setRestriction r c = c {restriction = Just r}

-- | Set the Order of a Criteria (sql order by clause)
setOrder :: [Order] -> Criteria -> Criteria
setOrder o c = c {ordering = o}

-- | Set the Extra of a Criteria (sql limit, offset, etc.)
addExtra :: String -> Criteria -> Criteria
addExtra e c = c { extra = e : extra c }

-- | Set the Projection of a Selct (sql select clause)
setProjection :: [Projection] -> Query -> Query
setProjection p c = c {projection = p}

-- * Query functions

-- | Create a empty Select
newSelect :: Query
newSelect = Select Nothing [] [] [] Nothing newCriteria

-- | Set the select Option (ALL, DISTINCT)
setOption :: String -> Query -> Query
setOption o q = q {option = Just o}

-- | Set the criteria
setCriteria :: Criteria -> Query -> Query
setCriteria c q = q {criteria = c}

-- | apply a funktion to the containing Criteria
modifyCriteria :: (Criteria -> Criteria) -> Query -> Query
modifyCriteria f q = q {criteria = f $ criteria q}

-- | Set a limit for the query
addLimit :: Int -> Int -> Criteria -> Criteria
addLimit start cnt = addExtra $ "LIMIT " ++ show start ++ ", " ++ show cnt

-- | Set the Tables of a Criteria (sql where clause)
setTables :: [TableName] -> Query -> Query
setTables t c = c {tables = t}

-- | Set the Grouoing of a Criteria (sql group by clause)
setGrouping :: [ColumnName] -> Query -> Query
setGrouping g c = c {grouping = g}

-- | Set the Restriction of a Criteria (sql having clause)
setHaving :: Restriction -> Query -> Query
setHaving h c = c {having = Just h}

-- * Manipulation functions

-- | Create a new DELETE sql
newDelete :: TableName -> Manipulation
newDelete t = Delete t newCriteria

-- | Create a new UPDATE sql
newUpdate :: TableName -> Manipulation
newUpdate t = Update t [] newCriteria

-- | Create a new INSERT sql
newInsert :: TableName -> Manipulation
newInsert t = Insert t []

-- | Set the criteria
setManipulationCriteria :: Criteria -> Manipulation -> Manipulation
setManipulationCriteria c m = m {manipulationCriteria = c}

-- | apply a funktion to the containing Criteria
modifyManipulationCriteria :: (Criteria -> Criteria) -> Manipulation -> Manipulation
modifyManipulationCriteria _ m@(Insert _ _) = m
modifyManipulationCriteria f q = q {manipulationCriteria = f $ manipulationCriteria q}

-- | set the Table name to a different value
setTable :: TableName -> Manipulation -> Manipulation
setTable t c = c {table = t}

-- | set the Column value combination for a UPDATE sql
setSet :: [(ColumnName, SqlValue)] -> Manipulation -> Manipulation
setSet s u@(Update _ _ _) = u {set = s}
setSet _ m = m

-- | set the Column value combination for a INSERT sql
setValues :: [(ColumnName, SqlValue)] -> Manipulation -> Manipulation
setValues v u@(Insert _ _) = u {values = v}
setValues _ m = m
