{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database
import Control.Monad (forever)
import Data.Text     (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Database.SQLite.Simple       as SQL
import qualified Network.Socket               as NS
import qualified Network.Socket.ByteString    as BS


returnUsers :: SQL.Connection -> NS.Socket -> IO ()
returnUsers dbConn soc = do
  putStrLn $ "Listing all users"
  rows <- getAllUsers dbConn
  let users = T.unlines (map username rows)
  BS.sendAll soc (TE.encodeUtf8 users)

formatUser :: User -> Text
formatUser (User _ user sh dir name _) =
  T.unlines $ zipWith T.append headers values
  where headers = ["Login: ", "Name: ", "Directory: ", "Shell: "]
        values  = [user, name, dir, sh]

returnUser :: SQL.Connection -> NS.Socket -> Text -> IO ()
returnUser dbConn soc uname = do
  putStrLn $ "Searching user: " ++ show uname
  user <- getUser dbConn (T.strip uname)
  let resp = case user of
               Nothing -> T.append "Username not found: " uname
               Just u  -> formatUser u
  BS.sendAll soc (TE.encodeUtf8 resp)

handleRequest :: SQL.Connection -> NS.Socket -> IO ()
handleRequest dbConn soc = do
  msg <- BS.recv soc 1024
  let name = T.strip $ TE.decodeUtf8 msg
  if T.null name then
    returnUsers dbConn soc
  else
    returnUser dbConn soc name

handleRequests :: SQL.Connection -> NS.Socket -> IO ()
handleRequests dbConn sock = forever $ do
  (conn, addr) <- NS.accept sock
  putStrLn $ "Connection established: " ++ show addr
  handleRequest dbConn conn
  NS.close conn


main :: IO ()
main = NS.withSocketsDo $ do

  let hints = Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]})
  let hostname = Nothing
  let svcname = Just "79"
  addrinfos <- NS.getAddrInfo hints hostname svcname

  let serveraddr = head addrinfos
  sock <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol
  NS.bind sock (NS.addrAddress serveraddr)
  NS.listen sock 1
  dbConn <- SQL.open databaseFile
  handleRequests dbConn sock
  SQL.close dbConn
  NS.close sock
