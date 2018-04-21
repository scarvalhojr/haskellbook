{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy             as TL (pack)
import qualified Data.Map                   as M  (Map, empty, findWithDefault,
                                                   insert)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.IO.Class           (liftIO)
import           System.Environment               (getArgs)
import           Data.IORef                       (IORef, newIORef, writeIORef,
                                                   readIORef)
import           Web.Scotty.Trans                 (ScottyT, ActionT, scottyT,
                                                   get, param, html)


data Config = Config { counts :: IORef (M.Map Text Integer)
                     , prefix :: Text }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (m', v)
  where v  = 1 + M.findWithDefault 0 k m
        m' = M.insert k v m

app :: Scotty ()
app = get "/:key" $ do
  config <- lift ask
  unprefixed <- param "key"
  let key' = mappend (prefix config) unprefixed
      refs = counts config
  (refs', newInteger) <- liftIO $ bumpBoomp key' <$> readIORef refs
  liftIO $ writeIORef refs refs'
  html $ mconcat ["<h1>Success! Count was: " , TL.pack $ show newInteger,
                  "</h1>" ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
