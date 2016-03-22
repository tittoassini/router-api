{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DeriveGeneric #-}
import           Control.Applicative
import           Control.Exception             (SomeException, catch, handle)
import           Data.Typed
import qualified Data.ByteString.Lazy          as BL
import           Data.Word                     (Word16)
import           Network.Router.API
import           System.Log.Logger
import           Control.Concurrent            (threadDelay)
import  qualified Network.WebSockets as WS
import           Network.WebSockets hiding (runClient,send,receive)
import           Network.WebSockets.Connection(sendCloseCode)
import Control.Concurrent.Async

-- import Data.Chronograph
-- import Criterion.Types hiding (measure)
-- import Criterion.Measurement
-- mm = runBenchmark (Benchmarkable $ print) 1

t = main

-- TODO: test by sending incorrect router value
main = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO

    tasks <- concat <$> sequence [wsTest
                                 ,byTypeTest [True,False,True] 2
                                 ]
    mapM_ wait tasks

    -- -- let numDeviceMsgs = 3
    -- -- m <- run $ master numDevices numDeviceMsgs
    -- -- devices <- mapM (run . device numDevices numDeviceMsgs) [1..numDevices]
    -- -- let t2 = m:devices

  where

    msg1 = BL.pack [1..40]

    msg n = BL.pack $ [fromIntegral n] --  : [1..40]

    msgL l = BL.pack . take l . concat . repeat $ [0..255]

    largeMsg = msgL 1000000

    -- master :: Int -> ClientApp Bool
    -- master numDevices numDeviceMsgs conn = do
    --   protocol conn $ NamedHub "huba"
    --   recMsgs conn $ numDevices*numDeviceMsgs
    --   return True

    -- device numDevices numDeviceMsgs id conn = do
    --    protocol conn $ NamedHub "huba"
    --    -- sendBinaryData conn (encode $ Named ("device"++show n) $ GeoPos (n*3.3) (n*5.5))
    --    threadDelay $ secs 1
    --    mapM_ (sendBinaryData conn . msg) [1 .. numDeviceMsgs]

    --    recMsgs conn $ (numDevices-1)*numDeviceMsgs
    --    return True

    -- Test low level protocol
    -- WebSockets should support up to 2**64 bytes long messages.
    -- wsTest :: ClientApp Bool
    wsTest = (:[]) <$> wsTest_
    wsTest_ = run (Echo False) $ \conn -> do
      let sendRec msg = do
            sendBinaryData conn msg
            msgRet <- receiveData conn
            threadDelay 0 -- 10000
            return $ msg == msgRet

      dbgS "wsTest"
      -- protocol conn $ Echo False
      -- sendBinaryData conn (serialize $ typedValue $ Echo False) -- True)

      dbgS "wsTest1"
      echoOK <- sendRec $ msgL $ 100 --10 * 1000 * 1000000
      dbgS "wsTest2"

      -- About 10K round trips per sec, locally.
      -- chronoIO (mapM (const $ sendRec msg1) [1..10]) >>= chronoPrint "Send Rec Time"

      mapM (const $ sendRec msg1) [1..10]
      dbgS "wsTest3"
      let pingMsg = "What's a fish without an eye?" :: BL.ByteString
      sendPing conn pingMsg
      ControlMessage (Pong pongMsg) <- WS.receive conn

      let closeCode = 11
      let closeMsg = ("Bye Byte" :: BL.ByteString)
      sendCloseCode conn closeCode closeMsg
      ce <- expectCloseException conn

      return $ echoOK && pingMsg==pongMsg && ce == Right (closeCode,closeMsg)

run router app = async $ do
   r <- runClient def router app
   dbg ["RUN RESULT",show r]

byTypeTest :: forall a. (Model a,Typed a,Flat a,Show a) => [a] -> Int -> IO [Async ()]
byTypeTest vs numDevices = mapM (\n -> run (byType (Proxy :: Proxy a)) $ \conn -> byTypeClient numDevices vs n conn) [1..numDevices]

byTypeClient :: forall a. (Typed a,Flat a,Show a) => Int -> [a] -> Int -> Connection -> IO [a]
byTypeClient numDevices vs id conn = do
  threadDelay $ secs 1 -- make sure all clients are connected or we will miss some messages.
  mapM_ (send conn) vs
  mapM (\n-> receive conn) [1 .. (numDevices-1)*length vs]

recMsgs :: Connection -> Int -> IO [BL.ByteString]
recMsgs conn n = mapM (\n -> receiveData conn >>= \msg -> dbg ["RCV",show n,show msg] >> return msg) [1 .. n]

expectCloseException :: Connection -> IO (Either String (Word16, BL.ByteString))
expectCloseException conn = act `catch` handler
    where
        act = do
          msg <- receiveDataMessage conn
          return . Left . show . ("Unexpected data message " ++) . show $ msg
        handler (CloseRequest i msg) = return $ Right (i,msg)
        handler e = return $ Left (show e)

secs = (* 1000000000)


-- Data model for a simple chat system
data Msg = Msg {fromUser::User
               ,subject::Subject
               ,content::Content}
         deriving (Eq, Ord, Read, Show, Generic)

type User = String

-- Hierarchical subject
-- Example: Subject ["Haskell","Meeting","Firenze"]
data Subject = Subject [String] deriving (Eq, Ord, Read, Show, Generic)

-- Different kinds of contents
data Content =
              -- Basic text message
              TextMsg String
              | Join
 deriving (Eq, Ord, Read, Show, Generic) 

instance Flat Msg
instance Flat Subject
instance Flat Content

instance Model Msg
instance Model Subject
instance Model Content


