module Network.Router.API.Pipes(
  --module Pipes
   runEffect,(>->),yield,for,await,lift
  ,pipeIn,pipeOut
  ) where

import Network.Router.API.Types
import Network.Router.API.Util
import Pipes

pipeIn :: (Show a, MonadIO m,Flat a) => Connection a -> Producer a m ()
pipeIn conn = loop
     where
       loop = do
         v <- liftIO $ receive conn
         yield v
         loop

pipeOut :: (Show a, MonadIO m,Flat a) => Connection a -> Consumer a m ()
pipeOut conn = loop
     where
       loop = do
         v <- await
         liftIO $ send conn v
         loop
