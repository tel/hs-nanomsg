{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Network.Nanomsg.C.Address where

import Prelude hiding (print)

import Foreign.C.Types (CInt)

import Data.Monoid
import Data.Textual
import qualified Text.Printer as Pr
import qualified Text.Printer.Numerals as Pr
import qualified Text.Parser.Char as Pa
import qualified Network.IP.Addr as IP

import qualified Filesystem.Path.CurrentOS as P

import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Text (Text)


import Network.Nanomsg.C.Syms

sockAddrMax :: CInt
sockAddrMax = getSym "NN_SOCKADDR_MAX"

data Address ty where
  Inproc :: Text          -> Address ty
  IPC    :: P.FilePath    -> Address ty
  -- ^ Note: this MUST be a valid (printable) 'P.FilePath' else it'll
  -- cause weird runtime *printing* behavior. It must also have a
  -- basename---a smart constructor ought to build a default basename
  -- in.
  TCP    :: TCPAddress ty -> Address ty

instance Printable (Address ty) where
  print (Inproc name) = Pr.text "inproc://" <> Pr.text name
  print (IPC path)    = Pr.text "ipc://" <> Pr.text (P.encode path)
  print (TCP addy)    = Pr.text "tcp://" <> print addy

-- | 'Listen' type TCP addresses are ones that can only be used for
-- 'Nanomsg.Network.C.bind'.
data Listen

-- | Listen type TCP addresses are ones that can only be used for
-- 'Nanomsg.Network.C.bind'.
data Broadcast

-- | A network interface on the local machine to target.
data TCPInterfacePart ty where
  StarI  ::               TCPInterfacePart Listen
  -- ^ Wildcard interfaces, @tcp://*@ are only usable locally
  IPv4I  :: IP.IP4 -> TCPInterfacePart ty
  IPv6I  :: IP.IP6 -> TCPInterfacePart ty
  NamedI :: Text   -> TCPInterfacePart ty
  -- ^ Named interfaces are OS specific, but we cannot do much
  -- checking. This is one great way to have an address failure: using
  -- an undefined interface name.

instance Printable (TCPInterfacePart ty) where
  print StarI      = Pr.text "*"
  print (IPv4I ip) = print ip
  print (IPv6I ip) = print ip
  print (NamedI n) = Pr.text n

-- | A network address to reach out to, local or foreign.
data TCPAddressPart ty where
  IPv4A :: IP.IP4  -> TCPAddressPart ty
  IPv6A :: IP.IP6  -> TCPAddressPart ty
  DNSA  :: Text    -> TCPAddressPart Broadcast
  -- ^ An address requiring a domain lookup.

instance Printable (TCPAddressPart ty) where
  print (IPv4A ip) = print ip
  print (IPv6A ip) = print ip
  print (DNSA n)   = Pr.text n

type Port = Int

-- | A TCP-style network address
data TCPAddress ty where
  Bind    :: TCPInterfacePart Listen
             -> Maybe Port
             -> TCPAddress Listen
  Connect :: Maybe (TCPInterfacePart Broadcast)
             -> TCPAddressPart Broadcast
             -> Maybe Port
             -> TCPAddress Broadcast

prMayport Nothing  = Pr.text T.empty
prMayport (Just p) = Pr.text ":" <> Pr.nnDecimal p

instance Printable (TCPAddress ty) where
  print (Bind int mayport)                = Pr.char '[' <> print int  <> Pr.char ']' <> prMayport mayport
  print (Connect Nothing    addy mayport) = Pr.char '[' <> print addy <> Pr.char ']' <> prMayport mayport
  print (Connect (Just int) addy mayport) =
       Pr.char '[' <> print int  <> Pr.char ']' <> Pr.char ';'
    <> Pr.char '[' <> print addy <> Pr.char ']' <> prMayport mayport
  
-- NN_SOCKADDR_MAX
-- parseAddress :: ByteString -> Parsed Address
-- parseAddress = undefined

-- toBS :: Address -> ByteString
-- toBS = undefined

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
