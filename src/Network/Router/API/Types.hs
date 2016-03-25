{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
module Network.Router.API.Types(
  module Data.Typed
  ,Config(..)
  ,def
  ,ByType(..),byType
  ,Echo(..)
  ,ByPattern(..),byPattern
--  ,module Data.Pattern
  ) where

import           Data.Default.Class
import           Data.Pattern hiding (Con,Var)
import           Data.Typed

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
-- Clients:
-- send messages of the given type
-- receive all messages of the same type sent by other agents
data ByType = ByType AbsType deriving (Eq, Ord, Show, Generic)

instance Flat ByType
instance Model ByType

-- |Smart constructor
byType :: Model a => Proxy a -> ByType
byType proxy = ByType (absType proxy)

-- |A router index by a pattern of a given type:
-- Clients:
-- send messages of the given type
-- receive all messages of the same type, that matches the given pattern, sent by other agents

data ByPattern = ByPattern AbsType (Pattern WildCard) deriving (Eq, Ord, Show, Generic)

instance Flat ByPattern
instance Model ByPattern

-- |Smart constructor
byPattern :: Model a => Proxy a -> Pattern WildCard -> ByPattern
byPattern proxy = ByPattern (absType proxy)


