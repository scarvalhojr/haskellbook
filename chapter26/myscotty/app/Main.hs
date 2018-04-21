
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty                     (scotty, get, param, html)
import Web.Scotty.Internal.Types      (ActionT(..))
import Control.Monad.Trans.Class      (MonadTrans, lift)
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Trans.Except     (ExceptT(..))
import Control.Monad.Trans.Reader     (ReaderT(..))
import Control.Monad.Trans.State.Lazy (StateT(..))
import Control.Monad                  (liftM)
import Data.Monoid                    (mconcat)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

liftStateT :: Monad m => m a -> StateT s m a
liftStateT m = StateT $ \s -> do
  a <- m
  return (a, s)

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"

    lift (putStrLn "hello1")

    (ActionT . lift . lift . lift) (putStrLn "hello2")

    (ActionT
     . (ExceptT . liftM Right)
     . lift
     . lift) (putStrLn "hello3")

    (ActionT
     . (ExceptT . fmap Right)
     . liftReaderT
     . lift) (putStrLn "hello4")

    (ActionT
     . (ExceptT . fmap Right)
     . liftReaderT
     . liftStateT) (putStrLn "hello5")

    liftIO (putStrLn "hello6")

    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
