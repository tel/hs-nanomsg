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
-- Module      : Network.Nanomsg.C.Transports
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Nanomsg.C.Transports (

  -- * Transports

  -- Transports are mechanisms by which sockets connect to one another
  -- through @bind@ and @connect@.

  Transport (..), transTCP, transIPC, transINPROC,

  ) where

import Foreign (Storable)
import Foreign.C.Types
import Network.Nanomsg.C.Syms

-- Transports
-------------

newtype Transport = Transport CInt
                  deriving (Eq, Storable)

-- | TCP transport allows for passing message over the network using
-- simple reliable one-to-one connections. TCP is the most widely used
-- transport protocol, it is virtually ubiquitous and thus the
-- transport of choice for communication over the network.
-- 
-- When binding a TCP socket address of the form
-- @tcp://interface:port@ should be used. Port is the TCP port number
-- to use. Interface is one of the following (optionally placed within
-- square brackets):
-- 
-- Asterisk character (*) meaning all local network interfaces.
-- 
-- * IPv4 address of a local network interface in numeric form
--   (@192.168.0.111@).
-- 
-- * IPv6 address of a local network interface in numeric form (@::1@).
-- 
-- * Interface name, as defined by operating system.
-- 
-- When connecting a TCP socket address of the form
-- tcp://interface;address:port should be used. Port is the TCP port
-- number to use. Interface is optional and specifies which local
-- network interface to use. If not specified, OS will select an
-- appropriate interface itself. If specified it can be one of the
-- following (optionally placed within square brackets):
-- 
-- * IPv4 address of a local network interface in numeric form
--   (@192.168.0.111@).
-- 
-- * IPv6 address of a local network interface in numeric form (@::1@).
-- 
-- * Interface name, as defined by operating system (@eth0@).
-- 
-- Finally, address specifies the remote address to connect to. It can
-- be one of the following (optionally placed within square brackets):
-- 
-- * IPv4 address of a remote network interface in numeric form
--   (@192.168.0.111@).
-- 
-- * IPv6 address of a remote network interface in numeric form (@::1@).
-- 
-- * The DNS name of the remote box.
transTCP :: Transport
transTCP = Transport (getSym "NN_TCP")

-- | In-process transport allows to send messages between threads or
-- modules inside a process. In-process address is an arbitrary
-- case-sensitive string preceded by @inproc://@ protocol
-- specifier. All in-process addresses are visible from any module
-- within the process. They are not visible from outside of the
-- process.
-- 
-- The nature of in-process transport makes it easy to pass pointers
-- between threads instead of actual data. This is, however,
-- considered a bad application design and violates the scalable
-- share-nothing architecture. If you do pass pointers among threads,
-- synchronising thread access to shared data becomes your
-- responsibility. Such design also prevents moving the thread into
-- different process or machine once the need arises. As a rule of the
-- thumb, don’t pass pointers among threads unless you know what you
-- are doing.
-- 
-- The overall buffer size for an inproc connection is determined by
-- @NN_RCVBUF@ socket option on the receiving end of the
-- connection. @NN_SNDBUF@ socket option is ignored. In addition to
-- the buffer, one message of arbitrary size will fit into the
-- buffer. That way, even messages larger than the buffer can be
-- transfered via inproc connection.
transINPROC :: Transport
transINPROC = Transport (getSym "NN_INPROC")

-- | Inter-process transport allows for sending messages between
-- processes within a single box. The implementation uses native IPC
-- mechanism provided by the local operating system and the IPC
-- addresses are thus OS-specific.
-- 
-- On POSIX-compliant systems, UNIX domain sockets are used and IPC
-- addresses are file references. Note that both relative
-- (@ipc://test.ipc@) and absolute (@ipc:///tmp/test.ipc@) paths may
-- be used. Also note that access rights on the IPC files must be set
-- in such a way that the appropriate applications can actually use
-- them.
-- 
-- On Windows, named pipes are used for IPC. IPC address is an
-- arbitrary case-insensitive string containing any character except
-- for backslash. Internally, address @ipc://test@ means that named
-- pipe @\\.\pipe\test@ will be used.
transIPC :: Transport
transIPC = Transport (getSym "NN_IPC")

instance Show Transport where
  show p
    | p == transTCP     = "Transport {TCP}"
    | p == transINPROC  = "Transport {Inproc}"
    | p == transIPC     = "Transport {IPC}"
    | otherwise         = "Transport {unknown}"
