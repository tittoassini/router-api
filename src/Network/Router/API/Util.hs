{-# LANGUAGE OverloadedStrings ,NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Network.Router.API.Util(
  liftIO,forever,when,unless,try,SomeException,threadDelay,seconds
  ,dbg,warn,err,dbgS,logLevel
  ,runClient,runClientForever,send,receive,sendMsg,receiveMsg
  ,protocol
  ,module X
  ) where

import           Control.Concurrent
import           Control.Exception        (SomeException, fromException, handle,
                                            try)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy     as L
import           Data.Flat
import           Data.Typed
import           Network.Router.API.Types
import qualified Network.WebSockets       as WS
import           System.Log.Logger        as X
--import           Control.Concurrent.Async as X

logLevel = updateGlobalLogger rootLoggerName . setLevel

-- Protect against crashes, restart on failure
runClientForever :: (Model router, Flat router) => Config -> router -> (WS.Connection -> IO a) -> IO ()
runClientForever cfg router op = forever $ do
     Left (ex :: SomeException) <- try $ runClient cfg router $ \conn -> do

       liftIO $ dbgS "connected"
       op conn

     -- Something went wrong, wait a few seconds and restart
     dbg ["Exited loop with error",concat ["'",show ex,"'"],"retrying in a bit."]
     threadDelay $ seconds 5

seconds = (* 1000000)

runClient :: (Model router,Flat router) => Config -> router -> (WS.Connection -> IO a) -> IO a
runClient cfg router client = runWSClient cfg (\conn -> protocol conn router >> client conn)

-- Automatically close sockets on App exit
runWSClient :: Config -> WS.ClientApp a -> IO a
runWSClient cfg app = do
     WS.runClientWith (ip cfg) (port cfg) (path cfg) opts [("Sec-WebSocket-Protocol", "quid2.net")] $ \conn -> do
       WS.forkPingThread conn 20 -- Keep connection alive avoiding timeouts
       app conn
       --WS.sendClose conn (1000::Int)
   where
     opts = WS.defaultConnectionOptions -- { WS.connectionOnPong = dbgS "gotPong"}

protocol conn =  send conn . typedBytes

send :: (Show a,Flat a) => WS.Connection -> a -> IO ()
send conn v = do
  WS.sendBinaryData conn $ flat v
  dbg ["sent",show v]

receive conn = do
   Right v <- unflat <$> WS.receiveData conn
   dbg ["received",show v]
   return v

sendMsg :: WS.Connection -> L.ByteString -> IO ()
sendMsg = WS.sendBinaryData

receiveMsg :: WS.Connection -> IO L.ByteString
receiveMsg conn = WS.receiveData conn

dbgS = debugM "Hub"
dbg = liftIO . dbgS . unwords
err = liftIO . errorM "Hub" . unwords
warn = liftIO . warningM "Hub" . unwords
