{-# LANGUAGE DeriveGeneric #-}
module Network.Router.API.Types(
  module Data.Typed
  ,Config(..)
  ,def
  ,ByType(..),byType
  ,Echo(..)
  ) where

import Data.Typed
import Data.Default.Class

-- |General client configuration
data Config = Config {ip::String,port::Int,path::String}

instance Default Config where def = Config "quid2.net" 8080 "/ws"

---------------- Routers

-- |Echo router: any value sent in is returned verbatim to the sender.
-- Caller can specify if received messages should be logged (for debugging purposes)
data Echo = Echo {echoDebug::Bool} deriving (Eq, Ord ,Show ,Generic)
instance Flat Echo
instance Model Echo

-- |A router indexed by type:
-- Clients can send messages of the given type
-- and receive all messages of the same type sent by other agents
data ByType = ByType AbsType deriving (Eq, Ord, Show, Generic)

instance Flat ByType
instance Model ByType

-- |Smart constructor
byType :: Model a => Proxy a -> ByType
byType proxy = ByType (absType proxy) 
