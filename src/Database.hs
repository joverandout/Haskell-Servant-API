{-# LANGUAGE OverloadedStrings #-}


module Database where
    
import Database.SQLite.Simple
import qualified Data.Text as T
import DataTypes

--DATABASE STUFF STARTS HERE
data TestField = TestField Int T.Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field


test = do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
  rowId <- lastInsertRowId conn
  executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
  close conn

test2 = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
  close conn

testUsersDatabase userr = do
  conn <- open "test.db"
  execute conn "INSERT INTO users VALUES (?,?,?,?)" (userName userr, userEmail userr, userAge userr, userOccupation userr)
  close conn

testGetFromDB = do
  conn <- open "test.db"
  r <- query_ conn "SELECT * from users" :: IO [User]
  print r
  close conn
--DATABASE STUFF ENDS HERE