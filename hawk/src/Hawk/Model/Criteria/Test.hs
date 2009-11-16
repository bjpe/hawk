{-# LANGUAGE FlexibleContexts #-}
module Hawk.Model.Criteria.Test where

import qualified Database.HDBC as HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import System.Directory (removeFile)
import Hawk.Model.Criteria
import Hawk.Model.Criteria.Types
import Control.Monad (liftM)
import Data.Convertible

import Test.HUnit

type Connections = (HDBC.ConnWrapper, HDBC.ConnWrapper)

assertEqualM :: (Monad m, Eq a, Show a) => String -> m a -> m a -> m Assertion
assertEqualM s a b = do
    a' <- a
    b' <- b
    return $ assertEqual s a' b'

testDb1 :: String
testDb1 = "test1.db"

testDb2 :: String
testDb2 = "test2.db"

startUp :: IO Connections
startUp = do
    dbh1 <- liftM HDBC.ConnWrapper $ connectSqlite3 testDb1
    HDBC.run dbh1 "CREATE TABLE projects (\
                       \_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \description TEXT)" []
    HDBC.run dbh1 "CREATE TABLE areas (\
                       \_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \description TEXT,\
                       \comment TEXT,\
                       \project_id  TEXT)" []
    HDBC.commit dbh1
    dbh2 <- liftM HDBC.ConnWrapper $ connectSqlite3 testDb2
    HDBC.run dbh2 "CREATE TABLE projects (\
                       \_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \description TEXT)" []
    HDBC.run dbh2 "CREATE TABLE areas (\
                       \_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \description TEXT,\
                       \comment TEXT,\
                       \project_id  TEXT)" []
    HDBC.commit dbh2
    return (dbh1, dbh2)

stop :: Connections -> IO ()
stop (dbh1, dbh2) =
    HDBC.disconnect dbh1
    >> removeFile testDb1
    >> HDBC.disconnect dbh2
    >> removeFile testDb2

assertSql :: String -> (String, [SqlValue]) ->
    (String, [SqlValue]) -> Connections -> IO Assertion
assertSql msg query1 query2 (dbh1, dbh2) =
    assertEqualM msg
        (uncurry (HDBC.quickQuery dbh1) query1)
        (uncurry (HDBC.quickQuery dbh2) query2)


compareProject :: Connections -> IO Assertion
compareProject = assertSql "table project should be same"
    ("pragma table_info (projects)", [])
    ("pragma table_info (projects)", [])

compareAreas :: Connections -> IO Assertion
compareAreas = assertSql "table areas should be same"
    ("pragma table_info (areas)", [])
    ("pragma table_info (areas)", [])

insert :: (SqlValue,SqlValue,SqlValue) -> Connections -> IO Assertion
insert (a1,a2,a3) = assertSql "insert should be return the same"
    ("INSERT INTO areas (description, comment, project_id) VALUES (?, ?, ?)", [a1,a2,a3])
    (toExprPair $ Insert "areas" [("description", a1), ("comment", a2), ("project_id", a3)])

insertValues :: [Integer] -> [(SqlValue,SqlValue,SqlValue)]
insertValues = map (\x -> (toSql ("test" ++ show x), toSql ("test" ++ show x), toSql x))

select1 :: Connections -> IO Assertion
select1 = assertSql "select should return the same"
    ("SELECT description FROM areas WHERE _id < ?", [toSql (2::Integer)])
    (toExprPair $ Select Nothing [colP "description"] ["areas"] [] Nothing $ Criteria (Just (ltExpr (col "_id") (val (2::Integer)))) [] [])

selectOrder :: Connections -> IO Assertion
selectOrder = assertSql "select should return the same"
    ("SELECT * FROM areas ORDER BY project_id DESC", [])
    (toExprPair $ Select Nothing [] ["areas"] [] Nothing $ Criteria Nothing [desc "project_id"] [])

selectProjection :: Connections -> IO Assertion
selectProjection = assertSql "select column as"
    ("SELECT description AS des FROM areas", [])
    (toExprPair $ Select Nothing [asP "des" $colP "description"] ["areas"] [] Nothing $ Criteria Nothing [desc "project_id"] [])

selectCnt  :: Connections -> IO Assertion
selectCnt = assertSql "select column as"
    ("SELECT COUNT (*) FROM areas", [])
    (toExprPair $ Select Nothing [rowCountP] ["areas"] [] Nothing newCriteria)

select :: Connections -> IO Assertion
select = assertSql "select should return the same"
    ("SELECT * FROM areas", [])
    (toExprPair $ Select Nothing [] ["areas"] [] Nothing newCriteria)

delete1 :: Connections -> IO Assertion
delete1 = assertSql "delete shoul be return the same"
    ("DELETE FROM areas WHERE project_id > ?", [toSql (8::Integer)])
    (toExprPair $ Delete "areas" $ Criteria (Just (gtExpr (col "project_id") (val (8::Integer)))) [] [])

withConection :: Connections -> [Connections -> IO Assertion] -> Test
withConection d = TestList . map (\f -> TestCase $ assert $ f d)

tests :: [Connections -> IO Assertion]
tests = [ compareProject
        , compareAreas
        , select ]
        ++
        map insert (insertValues [1..10])
        ++
        [ select
        , select1
        , selectOrder
        , delete1
        , select ]

main :: IO ()
main = do
    d <- startUp
    runTestTT $ withConection d tests
    stop d
