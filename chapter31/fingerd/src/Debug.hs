module Main where

import Control.Monad (forever)
import qualified Network.Socket.ByteString as BS (recv, sendAll)
import qualified Network.Socket            as NS


logAndEcho :: NS.Socket -> IO ()
logAndEcho sock = forever $ do
  (conn, addr) <- NS.accept sock
  putStrLn $ "Connection accepted: " ++ show addr
  msg <- BS.recv conn 1024
  putStrLn $ "Message received:"
  print msg
  BS.sendAll conn msg
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
  logAndEcho sock
  NS.close sock
