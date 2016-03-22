module Network.Router.API.Pipes(
  --module Pipes
   runEffect,(>->),yield,for,await,lift
  ,pipeIn,pipeOut
  ) where

import Network.Router.API.Util
import Pipes

pipeIn conn = loop
     where
       loop = do
         v <- liftIO $ receive conn
         yield v
         loop

pipeOut conn = loop
     where
       loop = do
         v <- await
         liftIO $ send conn v
         loop
