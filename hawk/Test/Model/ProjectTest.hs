-- $Id: ProjectTest.hs 1004 2009-10-04 18:00:42Z inf6509 $
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Test.Model.ProjectTest where

import App.Model.Project as Project
import qualified App.Model.Area as Area

import Hawk.Model

import System.Directory (removeFile)
import Control.Monad (liftM)
import Control.Monad.Reader

import qualified Database.HDBC as HDBC
import Database.HDBC.Types (ConnWrapper(..))
import Database.HDBC.Sqlite3 (connectSqlite3)
import Control.Exception (finally)
import Data.Maybe(fromJust)

import Test.HUnit

type Env = ReaderT HDBC.ConnWrapper IO

instance (MonadIO m, MonadReader HDBC.ConnWrapper m) => MonadDB m where
  getConnection = ask

testDb :: String
testDb = "test.db"

testDb2 = "test2.db"

startUp :: String -> IO ConnWrapper
startUp db = do
    dbh <- HDBC.ConnWrapper `liftM` connectSqlite3 db
    HDBC.run dbh "CREATE TABLE projects (\
                       \_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \description TEXT)" []
    HDBC.run dbh "CREATE TABLE areas (\
                       \_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \description TEXT,\
                       \comment TEXT,\
                       \project_id  TEXT)" []
    HDBC.commit dbh
    return dbh

stop :: IO ()
stop = removeFile testDb

wrap :: IO ConnWrapper -> IO () -> Env a -> IO ()
wrap connIO final action = do
    con <- connIO
    finally (runReaderT action con >> HDBC.disconnect con) final

ioTest :: (Assertable a) => Env a -> Test
ioTest = TestCase . assert . wrap startUp stop

main :: IO Counts
main =
    runTestTT $ TestList $ map ioTest
      [ testInsertSelect
      , testInsertSelectOne
      , testInsertFind
      , testDelete
      , testDeleteByRestriction
      , testDeleteAll
      , testFindMany
      , testUpdate
      , testUpdateMany
      , testDeleteMany
      , testNoParent
      , testWithParent
      ]

testProject, testProject2, testProject3 :: Project
testProject  = Project {_id=1,description="abc"}
testProject2 = Project {_id=2,description="def"}
testProject3 = Project {_id=3,description="ghj"}

testArea1, testArea2 :: Area.Area
testArea1 = Area.Area {Area._id=1, Area.description="foo", Area.comment="bar", Area.project_id=Nothing}
testArea2 = Area.Area {Area._id=2, Area.description="foobar", Area.comment="barfoo", Area.project_id=Just 1}

insert3 :: MonadDB m => m ()
insert3 = do
    insertMany [testProject, testProject2, testProject3]
    return ()

testInsertSelect :: MonadDB m => m Assertion
testInsertSelect = do
    insert3
    p' <- Project.select $ ltExpr (Col "_id") (Val $ HDBC.toSql (4::Integer))
    return $ assertEqual "writen and reading should be equal" [testProject, testProject2, testProject3] p'
-- TODO the order is not requied but is tested

testInsertSelectOne :: MonadDB m => m Assertion
testInsertSelectOne = do
    insertId <- insertWithPK testProject2
    p' <- selectOne (undefined::Project) $ eqExpr (Col "_id") (Val $ HDBC.toSql (_id insertId)) -- :: m (Maybe Project)
    return $ assertEqual "writen and reading should be equal" (Just testProject2) p'

testInsertFind :: MonadDB m => m Assertion
testInsertFind = do
    insertId <- insertWithPK testProject2
    p' <- find (undefined::Project) (_id insertId) -- :: m (Maybe Project)
    return $ assertEqual "writen and reading should be equal" (Just testProject2) p'

testDelete :: MonadDB m => m Assertion
testDelete = do
    insertId <- insertWithPK testProject2
    delete insertId
    p' <- find (undefined::Project) (_id insertId) -- :: m (Maybe Project)
    return $ assertEqual "after delete, find should not find the row" Nothing p'

testDeleteByRestriction :: MonadDB m => m Assertion
testDeleteByRestriction = do
    insert3
    deleteByRestriction (undefined::Project) (ltExpr (Col "_id") (Val $ HDBC.toSql (4::Integer)))
    p' <- Project.select $ ltExpr (Col "_id") (Val $ HDBC.toSql (4::Integer))
    return $ assertEqual "after deleting no row should be selectet" [] p'

testDeleteAll :: MonadDB m => m Assertion
testDeleteAll = do
    insert3
    deleteAll (undefined::Project)
    p' <- Project.select $ ltExpr (Col "_id") (Val $ HDBC.toSql (4::Integer))
    return $ assertEqual "after deletingAll no row should be selectet" [] p'

testFindMany :: MonadDB m => m Assertion
testFindMany = do
    insert3
    p' <- Project.findMany [1..3]
    return $ assertEqual "findMany should find all tree rows" [testProject, testProject2, testProject3] p'

testUpdate :: MonadDB m => m Assertion
testUpdate = do
    insert testProject
    update Project {_id=1,description="abcd"}
    p' <- find (undefined::Project) 1 -- :: m (Maybe Project)find 1::m (Maybe Project)
    return $ assertEqual "after updating, the text should in description the updated one" "abcd" (description (fromJust p'))

testUpdateMany :: MonadDB m => m Assertion
testUpdateMany = do
    insert3
    updateMany [Project {_id=1,description="abcd"}, Project {_id=2,description="defg"}, Project {_id=3,description="ghji"}]
    p' <- Project.findMany [1..3]
    return $ assertEqual "after updating many, the text should in description the updated ones" ["abcd", "defg", "ghji"] (map description p')

testDeleteMany :: MonadDB m => m Assertion
testDeleteMany = do
    insert3
    deleteMany [testProject, testProject2, testProject3]
    p' <- Project.select $ ltExpr (Col "_id") (Val $ HDBC.toSql (4::Integer))
    return $ assertEqual "after deleting Many the rows should be empty" [] p'

testNoParent :: MonadDB m => m Assertion
testNoParent = do
    insert testArea1
    par <- Area.getProject testArea1
    return $ assertEqual "Parent of empty relation, should be Nothing" Nothing par

testWithParent :: MonadDB m => m Assertion
testWithParent = do
    insert testProject
    insert testArea2
    par <- Area.getProject testArea2
    return $ assertEqual "Parent of set relation, should be Just" (Just testProject) par
