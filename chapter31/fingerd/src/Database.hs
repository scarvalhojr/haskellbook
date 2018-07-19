{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database
  ( User(..)
  , databaseFile
  , getUser
  , getAllUsers )
  where

import Control.Exception (Exception, throwIO)
import Data.Text         (Text)
import Data.Typeable     (Typeable)
import Text.RawString.QQ (r)
import qualified Database.SQLite.Simple as SQL


databaseFile :: String
databaseFile = "finger.db"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

data User =
  User {
      userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
  } deriving (Eq, Show)

instance SQL.FromRow User where
  fromRow = User <$> SQL.field
                 <*> SQL.field
                 <*> SQL.field
                 <*> SQL.field
                 <*> SQL.field
                 <*> SQL.field

instance SQL.ToRow User where
  toRow (User i u s d n p) = SQL.toRow (i, u, s, d, n, p)


createUsersTableStatement :: SQL.Query
createUsersTableStatement = [r|
  CREATE TABLE IF NOT EXISTS users
    (id            INTEGER PRIMARY KEY AUTOINCREMENT,
     username      TEXT UNIQUE,
     shell         TEXT,
     homeDirectory TEXT,
     realName      TEXT,
     phone TEXT)
|]

insertUserStatement :: SQL.Query
insertUserStatement = [r|
  INSERT INTO users
  VALUES (?, ?, ?, ?, ?, ?)
|]

getAllUsersStatement :: SQL.Query
getAllUsersStatement = [r|
  SELECT * from users
|]

getUserStatement :: SQL.Query
getUserStatement = [r|
  SELECT * from users where username = ?
|]

getUser :: SQL.Connection -> Text -> IO (Maybe User)
getUser conn name = do
  results <- SQL.query conn getUserStatement (SQL.Only name)
  case results of
    []     -> return $ Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

getAllUsers :: SQL.Connection -> IO [User]
getAllUsers conn = SQL.query_ conn getAllUsersStatement

createDatabase :: IO ()
createDatabase = do
  conn <- SQL.open databaseFile
  SQL.execute_ conn createUsersTableStatement
  SQL.close conn
