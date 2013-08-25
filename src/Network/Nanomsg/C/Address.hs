
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

{-|

Typesafe addresses. This is the basis for an 'IsString' instance for
addresses.

# TCP

## bind

tcp://*:8000
tcp://192.168.0.111:8000
  - address might be invalid: "Can't assign requested address [49] (src/transports/tcp/btcp.c:372)"
tcp://::1:8000
tcp://eth0:8000

tcp://[*]:8000
  - This one seems to not work: "(-1) 19: Operation not supported by device"
    whereas without the brackets it works just fine
tcp://[192.168.0.111]:8000
tcp://[::1]:8000
tcp://[eth0]:8000

## connect

tcp://192.168.0.111;192.168.0.111:8000
tcp://192.168.0.111;::1:8000
tcp://192.168.0.111;google.com:8000
tcp://::1;192.168.0.111:8000
tcp://::1;::1:8000
tcp://::1;google.com:8000
tcp://eth0;192.168.0.111:8000
tcp://eth0;::1:8000
tcp://eth0;google.com:8000

tcp://[192.168.0.111];192.168.0.111:8000
tcp://[192.168.0.111];::1:8000
tcp://[192.168.0.111];google.com:8000
tcp://[::1];192.168.0.111:8000
tcp://[::1];::1:8000
tcp://[::1];google.com:8000
tcp://[eth0];192.168.0.111:8000
tcp://[eth0];::1:8000
tcp://[eth0];google.com:8000

tcp://192.168.0.111:8000
tcp://::1:8000
tcp://google.com:8000

-}

{-# LANGUAGE OverloadedStrings,
             StandaloneDeriving,
             TypeSynonymInstances,
             FlexibleInstances,
             GADTs #-}
module Network.Nanomsg.C.Address (
  Bind, Connect,
  Interface (..), Port, TCPAddress (..),
  TCP, Address (..),
  freshInprocAddress,
  ser,
  ) where

import Foreign.C.Types (CInt)

import qualified Network.Addresses as IP
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import System.Random
import Network.Nanomsg.C.Syms

import Network.Nanomsg.Util

-- | Empty type for phantom tagging 'Address'es which can only be
-- interpreted as local, binding addresses.
data Bind

-- | Empty type for phantom tagging 'Address'es which can only be
-- interpreted as remote, connecting addresses.
data Connect

-- | A local or remote 'Interface'. Some kinds of 'Interface' are
-- polymorphic while others are only useful for binding or connecting
-- alone.
data Interface ty where
  All   :: Interface Bind
  Named :: ByteString -> Interface Bind
  IP    :: IP.IP      -> Interface ty
  DNS   :: ByteString -> Interface Connect

deriving instance Eq (Interface ty)
deriving instance Show (Interface ty)

instance Serial (Interface ty) where
  ser All       = "*"
  ser (Named s) = s
  ser (IP a)    = ser a
  ser (DNS s)   = s

type Port = Int

instance Serial Port where
  ser = S8.pack . show

-- | 'Address'es reflect at the type level whether they're 'bind'able
data TCPAddress ty where
  Local  :: Interface Bind
            -> Port -> TCPAddress Bind
  Remote :: Maybe (Interface Connect) -> Interface Connect
            -> Port -> TCPAddress Connect

deriving instance Eq (TCPAddress ty)
deriving instance Show (TCPAddress ty)

instance Serial (TCPAddress ty) where
  ser (Local int port)             =
    ser int <> ":" <> ser port
  ser (Remote Nothing tar port)    =
    ser tar <> ":" <> ser port
  ser (Remote (Just int) tar port) =
    ser int <> ":" <> ser tar <> ":" <> ser port

data Inproc
data TCP
data IPC

data Address trans ty where
  Inproc :: ByteString    -> Address Inproc ty
  TCP    :: TCPAddress ty -> Address TCP ty
  IPC    :: FilePath      -> Address IPC ty

deriving instance Eq (Address trans ty)
deriving instance Show (Address trans ty)

-- | Generate a 20-byte random string to use as an inproc address.
freshInprocAddress :: IO (Address Inproc ty)
freshInprocAddress = fmap (Inproc . S.pack . take 30 . randoms) newStdGen 

instance Serial FilePath where
  ser = S8.pack
  
instance Serial (Address trans ty) where
  ser (Inproc name) = "inproc://" <> name
  ser (TCP addy)    = "tcp://"    <> ser addy
  ser (IPC path)    = "ipc://"    <> ser path

sockAddrMax :: CInt
sockAddrMax = getSym "NN_SOCKADDR_MAX"

-- | Attempt to build an inproc transport address. Can fail if longer
-- than 'sockaddrMax' less the length of @"inproc://"@.
-- inproc :: ByteString -> Parsed Address
-- inproc name = if S.length addr < (fromIntegral sockAddrMax)
--               then Parsed (Inproc addr)
--               else Malformed [] "inproc.tooLong"
--   where addr = "inproc://" <> name

-- | IPC addresses refer to particular files on a POSIX system, or are
-- just arbitrary strings on Windows. Here we treat them exclusively
-- as the more restrictive POSIX case.
-- ipc :: P.FilePath -> Parsed Address
-- ipc path = if isOk then Parsed (IPC path) else Malformed [] "ipc.badPath"
--   where isOk = and [ not (P.null path)
--                    , not (P.null $ P.basename path)
--                    ]

-- Parse a * then anInetAddrIP
-- tcp :: IP -> Parsed Address
-- tcp = Parsed . TCP_IP
