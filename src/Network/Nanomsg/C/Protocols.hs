-- Copyright 2013 Joseph Tel Abrahamson
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you
-- may not use this file except in compliance with the License. You
-- may obtain a copy of the License at
-- 
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied. See the License for the specific language governing
-- permissions and limitations under the License.

-- |
-- Module      : Network.Nanomsg.C.Protocols
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

{-# LANGUAGE DeriveDataTypeable,
             ExistentialQuantification #-}
module Network.Nanomsg.C.Protocols (

  -- * Protocol types

  -- Scalability protocols define the different operational modes of
  -- Nanomsg sockets. Each is its own datatype as the types appear
  -- later as parameters to other types.

  Protocol (..),

  -- Sometimes it's useful to have an existentially typed notion of a
  -- 'Protocol'
  AProtocol (..), forgetProtocol, discoverProtocol,

  -- * Protocols
  Pair (..), Req (..), Rep (..), Pub (..), Sub (..),
  Surveyor (..), Respondent (..), Source (..), Sink (..),
  Push (..), Pull (..), Bus (..),

  ) where

import Data.Data
import Foreign.C.Types
import Network.Nanomsg.C.Syms

-- Protocols
------------

-- | A scalability protocol.
class Typeable p => Protocol p where
  proValue :: p -> CInt

-- | Forget a protocol's type.
forgetProtocol :: Protocol p => p -> AProtocol
forgetProtocol p = AProtocol p (proValue p)

-- | Test to see if some 'AProtocol' is actually a particular kind of
-- protocol. If the result type @a@ could typecheck, then it'll be a
-- 'Just', else a 'Nothing'.
discoverProtocol :: (Protocol p) => AProtocol -> Maybe p
discoverProtocol (AProtocol p _) = cast p

-- | A protocol with its class information erased. It's possible to
-- get it back using 'cast', but generally this can only be used to
-- transmit the underlying C constant. Broadly, we could do this just
-- by downconverting to the C constant ahead of time, but this way
-- 'cast' takes care of upconversions.
data AProtocol = forall p . Protocol p => AProtocol p CInt
               deriving (Typeable)

instance Show AProtocol where show _ = "(some protocol)"
instance Eq AProtocol where
  (AProtocol _ c1) == (AProtocol _ c2) = c1 == c2

-- | Socket for communication with exactly one peer. Each party can
-- send messages at any time. If the peer is not available or send
-- buffer is full subsequent calls to nn_send(3) will block until it’s
-- possible to send the message.
data Pair = Pair deriving (Show, Typeable)

instance Protocol Pair where
  proValue _ = getSym "NN_PAIR"

-- | Used to implement the client application that sends requests and
-- receives replies.
data Req = Req deriving (Show, Typeable)

instance Protocol Req where
  proValue _ = getSym "NN_REQ"

-- | Used to implement the stateless worker that receives requests and
-- sends replies.
data Rep = Rep deriving (Show, Typeable)

instance Protocol Rep where
  proValue _ = getSym "NN_REP"

-- | This socket is used to distribute messages to multiple
-- destinations. Receive operation is not defined.
data Pub = Pub deriving (Show, Typeable)

instance Protocol Pub where
  proValue _ = getSym "NN_PUB"

-- | Receives messages from the publisher. Only messages that the
-- socket is subscribed to are received. When the socket is created
-- there are no subscriptions and thus no messages will be
-- received. Send operation is not defined on this socket. The socket
-- can be connected to at most one peer.
data Sub = Sub deriving (Show, Typeable)

instance Protocol Sub where
  proValue _ = getSym "NN_SUB"

-- | Used to send the survey. The survey is delivered to all the
-- connected respondents. Once the query is sent, the socket can be
-- used to receive the responses. When the survey deadline expires,
-- receive will return ETIMEDOUT error.
data Surveyor = Surveyor deriving (Show, Typeable)

instance Protocol Surveyor where
  proValue _ = getSym "NN_SURVEYOR"

-- | Use to respond to the survey. Survey is received using receive
-- function, response is sent using send function. This socket can be
-- connected to at most one peer.
data Respondent = Respondent deriving (Show, Typeable)

instance Protocol Respondent where
  proValue _ = getSym "NN_RESPONDENT"

-- | Allows to send messages to the central sink. Receive operation is
-- not implemented on this socket type. This socket can be connected
-- to at most one peer.
data Source = Source deriving (Show, Typeable)

instance Protocol Source where
  proValue _ = getSym "NN_SOURCE"

-- | Receives the messages from multiple sources. Send operation is
-- not defined on this socket type.
data Sink = Sink deriving (Show, Typeable)

instance Protocol Sink where
  proValue _ = getSym "NN_SINK"

-- | This socket is used to send messages to the cluster of
-- load-balanced nodes. Receive operation is not implemented on this
-- socket type.
data Push = Push deriving (Show, Typeable)

instance Protocol Push where
  proValue _ = getSym "NN_PUSH"

-- | This socket is used to implement a node within a load-balanced
-- cluster. It can be used to receive messages. Send operation is not
-- implemented on this socket type. This socket can be connected to at
-- most one peer.
data Pull = Pull deriving (Show, Typeable)

instance Protocol Pull where
  proValue _ = getSym "NN_PULL"

-- | Sent messages are distributed to all nodes in the
-- topology. Incoming messages from all other nodes in the topology
-- are fair-queued in the socket.
data Bus = Bus deriving (Show, Typeable)

instance Protocol Bus where
  proValue _ = getSym "NN_BUS"
