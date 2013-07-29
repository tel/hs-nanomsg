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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Network.Nanomsg.Constants
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- C imports and constants, isolated to a single file. This is the raw
-- material for interacting with the Nanomsg library and it should
-- pretty much all be abstracted away as soon as possible.

module Network.Nanomsg.C (

  -- * Basic API

  -- These are the primary functions for interacting with Nanomsg. The
  -- majority of the types exported from this module are used to work
  -- with these functions in a typesafe manner.
    
  newSocket, new, raw, close,
  setOpt,
  bind, connect, shutdown,
  
  -- * Error types

  -- Errors are semantically similar to POSIX errors, but Nanomsg is
  -- not guaranteed to be compatible with system POSIX error
  -- codes---it may redefine them for compatibility.
  --
  -- Furthermore, the system 'getErrno' may also not be
  -- compatible. Nanomsg exports its own 'nnGetErrno' for
  -- compatibility.
  
  Errno (), nnGetErrno,
  
  -- ** Error codes

  eOK, eADDRINUSE, eADDRNOTAVAIL, eAFNOSUPPORT, eAGAIN, 
  eBADF, eCONNREFUSED, eFSM, eINPROGRESS, eINTR, eINVAL, 
  eMFILE, eNAMETOOLONG, eNETDOWN, eNOBUFS, eNODEV, eNOPROTOOPT, 
  eNOTSOCK, eNOTSUP, ePROTO, ePROTONOSUPPORT, eTERM, eTIMEDOUT,

  -- * Option types

  -- Option types are for use with @nn_setsockopt@. They use phantom
  -- types to ensure that the value set for that option is correct.

  Option (), OptionTy (), OptionSet (),

  -- These are types useful for better specifying some default
  -- conditions for some options
  MaybeNeg (..), MaybeZero (..),

  -- ** Option values

  optLINGER, optSNDBUF, optRCVBUF, optSNDTIMEO, optRCVTIMEO, 
  optRECONNECT_IVL, optRECONNECT_IVL_MAX, optSNDPRIO, optSNDFD, 
  optRCVFD, optDOMAIN, optPROTOCOL, optRESEND_IVL, optSUBSCRIBE, 
  optUNSUBSCRIBE, optDEADLINE, optNODELAY,

  -- * Protocol types

  -- Scalability protocols define the different operational modes of
  -- Nanomsg sockets. Each is its own datatype as the types appear
  -- later as parameters to other types.

  Protocol (),

  -- Sometimes it's useful to have an existentially typed notion of a
  -- 'Protocol'
  AProtocol (), forgetProtocol, discoverProtocol,

  -- * Protocols
  Pair (..), Req (..), Rep (..), Pub (..), Sub (..),
  Surveyor (..), Respondent (..), Source (..), Sink (..),
  Push (..), Pull (..), Bus (..),

  -- * Transports

  -- Transports are mechanisms by which sockets connect to one another
  -- through @bind@ and @connect@.

  Transport (), transTCP, transIPC, transINPROC,

  -- * Addresses

  -- Addr.Address (),
  -- Addr.inproc, Addr.ipc, Addr.tcp, Addr.parseAddress, Addr.sockAddrMax,
  
  -- * Domains

  -- Domains are broad modes of operation of a socket. In particular,
  -- they let you choose to create a \"raw\" socket which is suitable
  -- for internal nodes in a topology.

  Domain (), domSP, domSPRAW,

  -- * Priorities

  -- Just a set of convenient constructors of thread priorities.

  Priority (), incPrio, decPrio, prioLowest, prioHighest,

  prio1,  prio2,  prio3,  prio4,  prio5,  prio6,  prio7,  prio8,
  prio9,  prio10, prio11, prio12, prio13, prio14, prio15, prio16,

  -- * The dynamic Nanomsg version
  version
  
  ) where

import Prelude hiding (FilePath)

import Control.Applicative
import Control.Monad

import Data.Time
import Data.Data
import Data.Monoid
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as Su

import Foreign         hiding (unsafePerformIO, new)
import Foreign.C.Types
import Foreign.C.String

import System.Posix.Types

import System.IO.Unsafe (unsafePerformIO)

import           Network.Nanomsg.C.Syms
import qualified Network.Nanomsg.C.Address as Addr

-- Version
----------

-- | The current version of Nanomsg.
version :: (Int, Int, Int)
version = ( fromIntegral $ getSym "NN_VERSION_MAJOR"
          , fromIntegral $ getSym "NN_VERSION_MINOR"
          , fromIntegral $ getSym "NN_VERSION_PATCH"
          )


-- Sockets
----------

-- | Sockets are just wrappers over file handles. They have a phantom
-- parameter which encodes their 'Protocol'.
newtype Socket a = Socket Fd deriving (Eq, Storable)

instance Show (Socket Pair) where
  show _ = "Socket{Pair}"
instance Show (Socket Req) where
  show _ = "Socket{Req}"
instance Show (Socket Rep) where
  show _ = "Socket{Rep}"
instance Show (Socket Pub) where
  show _ = "Socket{Pub}"
instance Show (Socket Sub) where
  show _ = "Socket{Sub}"
instance Show (Socket Surveyor) where
  show _ = "Socket{Surveyor}"
instance Show (Socket Respondent) where
  show _ = "Socket{Respondent}"
instance Show (Socket Source) where
  show _ = "Socket{Source}"
instance Show (Socket Sink) where
  show _ = "Socket{Sink}"
instance Show (Socket Push) where
  show _ = "Socket{Push}"
instance Show (Socket Pull) where
  show _ = "Socket{Pull}"


-- Errors
---------

-- | Nanomsg has its own version of 'Errno'.
newtype Errno = Errno CInt deriving (Eq, Storable)

-- | All good.
eOK             :: Errno
eOK             = Errno $ 0

-- | "Address already in use"
eADDRINUSE      :: Errno
eADDRINUSE      = Errno $ getSym "EADDRINUSE"

-- | "Can't assign requested address"
eADDRNOTAVAIL   :: Errno
eADDRNOTAVAIL   = Errno $ getSym "EADDRNOTAVAIL"

-- | "Address family not supported by protocol family"
eAFNOSUPPORT    :: Errno
eAFNOSUPPORT    = Errno $ getSym "EAFNOSUPPORT"

-- | "Resource temporarily unavailable"
eAGAIN          :: Errno
eAGAIN          = Errno $ getSym "EAGAIN"

-- | "Bad file descriptor"
eBADF           :: Errno
eBADF           = Errno $ getSym "EBADF"

-- | "Connection refused"
eCONNREFUSED    :: Errno
eCONNREFUSED    = Errno $ getSym "ECONNREFUSED"

-- | "Operation cannot be performed in this state"
eFSM            :: Errno
eFSM            = Errno $ getSym "EFSM"

-- | "Operation now in progress"
eINPROGRESS     :: Errno
eINPROGRESS     = Errno $ getSym "EINPROGRESS"

-- | "Interrupted system call"
eINTR           :: Errno
eINTR           = Errno $ getSym "EINTR"

-- | "Invalid argument"
eINVAL          :: Errno
eINVAL          = Errno $ getSym "EINVAL"

-- | "Too many open files"
eMFILE          :: Errno
eMFILE          = Errno $ getSym "EMFILE"

-- | "File name too long"
eNAMETOOLONG    :: Errno
eNAMETOOLONG    = Errno $ getSym "ENAMETOOLONG"

-- | "Network is down"
eNETDOWN        :: Errno
eNETDOWN        = Errno $ getSym "ENETDOWN"

-- | "No buffer space available"
eNOBUFS         :: Errno
eNOBUFS         = Errno $ getSym "ENOBUFS"

-- | "Operation not supported by device"
eNODEV          :: Errno
eNODEV          = Errno $ getSym "ENODEV"

-- | "Protocol not available"
eNOPROTOOPT     :: Errno
eNOPROTOOPT     = Errno $ getSym "ENOPROTOOPT"

-- | "Socket operation on non-socket"
eNOTSOCK        :: Errno
eNOTSOCK        = Errno $ getSym "ENOTSOCK"

-- | "Operation not supported"
eNOTSUP         :: Errno
eNOTSUP         = Errno $ getSym "ENOTSUP"

-- | "Protocol error"
ePROTO          :: Errno
ePROTO          = Errno $ getSym "EPROTO"

-- | "Protocol not supported"
ePROTONOSUPPORT :: Errno
ePROTONOSUPPORT = Errno $ getSym "EPROTONOSUPPORT"

-- | "Nanomsg library was terminated"
eTERM           :: Errno
eTERM           = Errno $ getSym "ETERM"

-- | "Operation timed out"
eTIMEDOUT       :: Errno
eTIMEDOUT       = Errno $ getSym "ETIMEDOUT"


-- | Get the current 'Errno' accounts to Nanomsg. This is not
-- guaranteed to be the same as 'getErrno'.
nnGetErrno :: IO Errno
nnGetErrno = nn_errno

-- | Print a descriptive string for a particular Nanomsg 'Errno'.
errnoString :: Errno -> String
errnoString (Errno 0) = "Error {No error}"
errnoString e         =
  "Error {"
  ++ (unsafePerformIO $ peekCString (nn_strerror e))
  ++ "}"
{-# NOINLINE errnoString #-}

instance Show Errno where
  show = errnoString

-- Socket-level Options
-----------------------

-- | Default instance used for many 'OptionTy's.
setItStorable :: Storable ty => ty -> (forall a. Ptr a -> CSize -> IO b) -> IO b
setItStorable s f = alloca $ \ptr -> do
  poke ptr s
  f ptr (fromIntegral $ sizeOf s)

-- | Default instance used for many 'OptionTy's. Returns 'Nothing' if
-- the size doesn't match what it's expected to (!).
-- getItStorable :: forall ty b. Storable ty =>
--                  (Ptr ty -> Ptr CSize -> IO b) -> IO (Maybe (ty, b))
-- getItStorable f = do
--   alloca $ \mainPtr -> do
--     alloca $ \sizePtr -> do
--       res <- f mainPtr sizePtr
--       sz  <- peek $ sizePtr
--       -- This scoped type variable is needed since we can't peek
--       -- mainPtr yet.
--       case sizeOf (undefined :: ty) == fromIntegral sz of
--         True  -> do
--           out <- peek mainPtr
--           return $ Just (out, res)
--         False -> return Nothing

-- | Class generalizing how option types are serialized for setting.
class OptionTy ty where
  setIt :: ty -> (forall a. Ptr a -> CSize -> IO b) -> IO b
  -- getIt :: (forall a. Ptr a -> Ptr CSize -> IO b) -> IO (ty, b)

instance OptionTy CInt where
  setIt = setItStorable

-- | Nanomsg always interprets times a 'CInt's of milliseconds
instance OptionTy NominalDiffTime where
  setIt t = setIt (round (fromRational (toRational t) * 1000 :: Double) :: CInt)

instance OptionTy Priority where
  setIt = setItStorable

instance OptionTy Fd where
  setIt = setItStorable

instance OptionTy Domain where
  setIt = setItStorable

instance OptionTy AProtocol where
  setIt (AProtocol _ i) = setIt i

instance (OptionTy a, Num a) => OptionTy (MaybeZero a) where
  setIt (MaybeZero m) f = case m of
    Nothing  -> setIt (0 :: CInt) f
    (Just i) -> setIt i f

-- | We're assuming that any negative value is a fine default, so @-1@
-- is a great choice.
instance (OptionTy a, Num a) => OptionTy (MaybeNeg a) where
  setIt (MaybeNeg m) f = case m of
    Nothing  -> setIt (-1 :: CInt) f
    (Just i) -> setIt i f

-- | Class dictating which Protocols can have which options set.
class OptionSet opt pro where

-- | Class dictating which Protocols can get which options.
class OptionGet opt pro where
  
-- Options for all sockets
  
instance OptionSet LINGER a
instance OptionSet SNDBUF a
instance OptionSet RCVBUF a
instance OptionSet SNDTIMEO a
instance OptionSet RCVTIMEO a
instance OptionSet RECONNECT_IVL a
instance OptionSet RECONNECT_IVL_MAX a
instance OptionSet SNDPRIO a

instance OptionGet LINGER a
instance OptionGet SNDBUF a
instance OptionGet RCVBUF a
instance OptionGet SNDTIMEO a
instance OptionGet RCVTIMEO a
instance OptionGet RECONNECT_IVL a
instance OptionGet RECONNECT_IVL_MAX a
instance OptionGet SNDPRIO a
instance OptionGet DOMAIN a
instance OptionGet PROTOCOL a

-- | This option can only be gotten
instance OptionGet SNDFD a
-- | This option can only be gotten
instance OptionGet RCVFD a

-- Options for all sockets (via the transport)

instance OptionSet NODELAY a
instance OptionGet NODELAY a

-- Options for specific socket protocols

instance OptionSet RESEND_IVL Req
instance OptionGet RESEND_IVL Req
-- | What would it mean to get this option?
instance OptionSet SUBSCRIBE Sub
-- | What would it mean to get this option?
instance OptionSet UNSUBSCRIBE Sub
instance OptionSet DEADLINE Surveyor
instance OptionGet DEADLINE Surveyor

-- | A 'Maybe'-alike which indicates that the default value is @-1@.
newtype MaybeNeg a = MaybeNeg { getMaybeNeg :: Maybe a }
                deriving (Eq, Show, Ord, Functor, Applicative,
                          Monad, Alternative, MonadPlus, Monoid)

-- | A 'Maybe'-alike which indicates that the default value is @0@.
newtype MaybeZero a = MaybeZero { getMaybeZero :: Maybe a }
                deriving (Eq, Show, Ord, Functor, Applicative,
                          Monad, Alternative, MonadPlus, Monoid)

-- | Kinds of options. Since options pass their values as @void *@
-- pointers, these are annotated with phantom types which must match
-- the passed option.
data Option a b = Option
                  !CInt
                  -- ^ Option type
                  !CInt
                  -- ^ Option level
                deriving (Eq)

solLevel :: CInt
solLevel = getSym "NN_SOL_SOCKET"

-- | Specifies how long should the socket try to send pending outbound
-- messages after nn_close() have been called, in
-- milliseconds. Negative value means infinite linger. The type of the
-- option is int. Default value is 1000 (1 second).
data LINGER
optLINGER            :: Option LINGER (MaybeNeg NominalDiffTime)
optLINGER            = Option (getSym "NN_LINGER") solLevel
                              
-- | Size of the send buffer, in bytes. To prevent blocking for
-- messages larger than the buffer, exactly one message may be
-- buffered in addition to the data in the send buffer. The type of
-- this option is int. Default value is 128kB.
data SNDBUF
optSNDBUF            :: Option SNDBUF CInt
optSNDBUF            = Option (getSym "NN_SNDBUF") solLevel

-- | Size of the receive buffer, in bytes. To prevent blocking for
-- messages larger than the buffer, exactly one message may be
-- buffered in addition to the data in the receive buffer. The type of
-- this option is int. Default value is 128kB.
data RCVBUF
optRCVBUF            :: Option RCVBUF CInt
optRCVBUF            = Option (getSym "NN_RCVBUF") solLevel

-- | The timeout for send operation on the socket, in milliseconds. If
-- message cannot be sent within the specified timeout, EAGAIN error
-- is returned. Negative value means infinite timeout. The type of the
-- option is int. Default value is -1.
data SNDTIMEO
optSNDTIMEO          :: Option SNDTIMEO (MaybeNeg NominalDiffTime)
optSNDTIMEO          = Option (getSym "NN_SNDTIMEO") solLevel

-- | The timeout for recv operation on the socket, in milliseconds. If
-- message cannot be received within the specified timeout, EAGAIN
-- error is returned. Negative value means infinite timeout. The type
-- of the option is int. Default value is -1.
data RCVTIMEO
optRCVTIMEO          :: Option RCVTIMEO (MaybeNeg NominalDiffTime)
optRCVTIMEO          = Option (getSym "NN_RCVTIMEO") solLevel

-- | For connection-based transports such as TCP, this option
-- specifies how long to wait, in milliseconds, when connection is
-- broken before trying to re-establish it. Note that actual reconnect
-- interval may be randomised to some extent to prevent severe
-- reconnection storms. The type of the option is int. Default value
-- is 100 (0.1 second).
data RECONNECT_IVL
optRECONNECT_IVL     :: Option RECONNECT_IVL NominalDiffTime
optRECONNECT_IVL     = Option (getSym "NN_RECONNECT_IVL") solLevel

-- | This option is to be used only in addition to NN_RECONNECT_IVL
-- option. It specifies maximum reconnection interval. On each
-- reconnect attempt, the previous interval is doubled until
-- NN_RECONNECT_IVL_MAX is reached. Value of zero means that no
-- exponential backoff is performed and reconnect interval is based
-- only on NN_RECONNECT_IVL. If NN_RECONNECT_IVL_MAX is less than
-- NN_RECONNECT_IVL, it is ignored. The type of the option is
-- int. Default value is 0.
data RECONNECT_IVL_MAX
optRECONNECT_IVL_MAX :: Option RECONNECT_IVL_MAX (MaybeZero NominalDiffTime)
optRECONNECT_IVL_MAX = Option (getSym "NN_RECONNECT_IVL_MAX") solLevel

-- | Retrieves outbound priority currently set on the socket. This
-- option has no effect on socket types that send messages to all the
-- peers. However, if the socket type sends each message to a single
-- peer (or a limited set of peers), peers with high priority take
-- precedence over peers with low priority. The type of the option is
-- int. Highest priority is 1, lowest priority is 16. Default value is
-- 8.
data SNDPRIO
optSNDPRIO           :: Option SNDPRIO Priority
optSNDPRIO           = Option (getSym "NN_SNDPRIO") solLevel

-- | Retrieves a file descriptor that is readable when a message can
-- be sent to the socket. The descriptor should be used only for
-- polling and never read from or written to. The type of the option
-- is same as the type of file descriptor on the platform. That is,
-- int on POSIX-complaint platforms and SOCKET on Windows. The
-- descriptor becomes invalid and should not be used any more once the
-- socket is closed. This socket option is not available for
-- unidirectional recv-only socket types.
data SNDFD
optSNDFD             :: Option SNDFD Fd
optSNDFD             = Option (getSym "NN_SNDFD") solLevel

-- | Retrieves a file descriptor that is readable when a message can
-- be received from the socket. The descriptor should be used only for
-- polling and never read from or written to. The type of the option
-- is same as the type of file descriptor on the platform. That is,
-- int on POSIX-complaint platforms and SOCKET on Windows. The
-- descriptor becomes invalid and should not be used any more once the
-- socket is closed. This socket option is not available for
-- unidirectional send-only socket types.
data RCVFD
optRCVFD             :: Option RCVFD Fd
optRCVFD             = Option (getSym "NN_RCVFD") solLevel

-- | Returns the domain constant as it was passed to nn_socket().
data DOMAIN
optDOMAIN            :: Option DOMAIN Domain
optDOMAIN            = Option (getSym "NN_DOMAIN") solLevel

-- | Returns the protocol constant as it was passed to nn_socket().
data PROTOCOL
optPROTOCOL          :: Protocol a => Option PROTOCOL a
optPROTOCOL          = Option (getSym "NN_PROTOCOL") solLevel

-- If set to 1, only IPv4 addresses are used. If set to 0, both IPv4
-- and IPv6 addresses are used. Default value is 1.
--
-- Not in the symbol map currently, so we'll ignore it.
-- 
-- optIPV4ONLY          :: Option Bool
-- optIPV4ONLY          = Option (getSym "NN_IPV4ONLY")


-- Protocol-level Options
-------------------------

-- | This option is defined on the full REQ socket. If reply is not
-- received in specified amount of milliseconds, the request will be
-- automatically resent. The type of this option is int. Default value
-- is 60000 (1 minute).
data RESEND_IVL
optRESEND_IVL :: Option RESEND_IVL NominalDiffTime
optRESEND_IVL = Option (getSym "NN_REQ_RESEND_IVL")
                       (proValue Req)

-- | Defined on full SUB socket. Subscribes for a particular
-- topic. Type of the option is string.
data SUBSCRIBE
optSUBSCRIBE :: Option SUBSCRIBE String
optSUBSCRIBE = Option (getSym "NN_SUB_SUBSCRIBE")
                      (proValue Sub)

-- | Defined on full SUB socket. Unsubscribes from a particular
-- topic. Type of the option is string.
data UNSUBSCRIBE
optUNSUBSCRIBE :: Option UNSUBSCRIBE String
optUNSUBSCRIBE = Option (getSym "NN_SUB_UNSUBSCRIBE")
                        (proValue Sub)

-- | Specifies how long to wait for responses to the survey. Once the
-- deadline expires, receive function will return ETIMEDOUT error and
-- all subsequent responses to the survey will be silently
-- dropped. The deadline is measured in milliseconds. Option type is
-- int. Default value is 1000 (1 second).
data DEADLINE
optDEADLINE :: Option DEADLINE NominalDiffTime
optDEADLINE = Option (getSym "NN_SURVEYOR_DEADLINE")
                     (proValue Surveyor)


-- Transport-level Options
--------------------------

-- | This option, when set to 1, disables Nagle’s algorithm. It also
-- disables delaying of TCP acknowledgments. Using this option
-- improves latency at the expense of throughput. Type of this option
-- is int. Default value is 0.
data NODELAY
optNODELAY :: Option NODELAY Bool
optNODELAY = Option (getSym "NN_TCP_NODELAY")
                    (getTransport transTCP)

instance Show (Option LINGER a) where show _            = "Socket Option {Linger}"
instance Show (Option SNDBUF a) where show _            = "Socket Option {Send Buffer Size}"
instance Show (Option RCVBUF a) where show _            = "Socket Option {Receive Buffer Size}"
instance Show (Option SNDTIMEO a) where show _          = "Socket Option {Send Timeout}"
instance Show (Option RCVTIMEO a) where show _          = "Socket Option {Receive Timeout}"
instance Show (Option RECONNECT_IVL a) where show _     = "Socket Option {Reconnect IVL}"
instance Show (Option RECONNECT_IVL_MAX a) where show _ = "Socket Option {Reconnect IVL Maximum}"
instance Show (Option SNDPRIO a) where show _           = "Socket Option {Send Priority}"
instance Show (Option SNDFD a) where show _             = "Socket Option {Send Fd}"
instance Show (Option RCVFD a) where show _             = "Socket Option {Receive Fd}"
instance Show (Option DOMAIN a) where show _            = "Socket Option {Domain}"
instance Show (Option PROTOCOL a) where show _          = "Socket Option {Protocol}"
-- instance Show (Option IPV4ONLY a) where show _          = "Socket Option {IPv4 Only}"
instance Show (Option RESEND_IVL a) where show _    = "Protocol Option {Req/Resend IVL}"
instance Show (Option SUBSCRIBE a) where show _     = "Protocol Option {Sub/Subscribe}"
instance Show (Option UNSUBSCRIBE a) where show _   = "Protocol Option {Sub/Unsubscribe}"
instance Show (Option DEADLINE a) where show _ = "Protocol Option {Surveyor/Deadline}"  
instance Show (Option NODELAY a) where show _       = "Transport Option {TCP/No delay}"

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

-- Transports
-------------

newtype Transport = Transport { getTransport :: CInt }
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

-- Domains
----------

newtype Domain = Domain CInt deriving (Eq, Storable)

-- | Standard full-blown SP socket.
domSP :: Domain
domSP = Domain (getSym "AF_SP")

-- | Raw SP socket. Raw sockets omit the end-to-end functionality
-- found in AF_SP sockets and thus can be used to implement
-- intermediary devices in SP topologies.
domSPRAW :: Domain
domSPRAW = Domain (getSym "AF_SP_RAW")

-- Priorities
-------------

newtype Priority = Priority CInt deriving (Eq, Ord, Storable)

prioLowest,  prio16, prio15, prio14, prio13, prio12, prio11, prio10, prio9 :: Priority
prioHighest, prio1,  prio2,  prio3,  prio4,  prio5,  prio6,  prio7,  prio8 :: Priority

prioLowest  = Priority 16
prio16      = Priority 16
prio15      = Priority 15
prio14      = Priority 14
prio13      = Priority 13
prio12      = Priority 12
prio11      = Priority 11
prio10      = Priority 10
prio9       = Priority 9
prio8       = Priority 8
prio7       = Priority 7
prio6       = Priority 6
prio5       = Priority 5
prio4       = Priority 4
prio3       = Priority 3
prio2       = Priority 2
prio1       = Priority 1
prioHighest = Priority 1

-- | Increase the priority by one notch. If it's already at the
-- highest priority then nothing happens.
incPrio :: Priority -> Priority
incPrio p@(Priority n) | p <= prioHighest = Priority (succ n)
                       | otherwise        = prioHighest

-- | Decrease the priority by one notch. If it's already at the
-- lowest priority then nothing happens.
decPrio :: Priority -> Priority
decPrio p@(Priority n) | p <= prioLowest = Priority (pred n)
                       | otherwise       = prioLowest

-- Endpoints
------------

-- | Endpoint IDs are generated by 'bind' and 'connect' and handed to
-- 'shutdown' to terminate them.
newtype Endpoint = Endpoint CInt deriving (Eq, Storable)


-- Real API
-----------

-- [ ] [?] EBADF       :: The provided socket is invalid.
-- [X] [T] ENOPROTOOPT :: The option is unknown at the level indicated.
-- [?] [T] EINVAL      :: The specified option value is invalid.
--                        What about negative numbers in sizes?
-- [ ] [U] ETERM       :: The library is terminating.

-- | This is the core, typesafe option setter. Note that option
-- setting is not idempotent (for instance, 'optSUBSCRIBE').
setOpt :: (OptionSet opt pro, OptionTy a) =>
          Option opt a -> a -> Socket pro -> IO Errno
setOpt (Option opt level) ty s = do
  res <- setIt ty (nn_setsockopt s level opt)
  case res < 0 of
    True -> nnGetErrno
    False -> return eOK

-- [X] [T] EAFNOSUPPORT :: Specified address family is not supported.
-- [X] [T] EINVAL       :: Unknown protocol.
-- [ ] [R] EMFILE       :: The limit on the total number of open SP sockets
--                         or OS limit for file descriptors has been reached.
-- [ ] [U] ETERM        :: The library is terminating.

-- | Creates a new socket.
newSocket :: Protocol p => Domain -> p -> IO (Socket p)
newSocket dom proto =
  do ret <- nn_socket dom (proValue proto)
     if ret < 0 then nnGetErrno >>= error . show else return (Socket (Fd ret))

-- | Creates a 'raw' socket
raw :: Protocol p => p -> IO (Socket p)
raw = newSocket domSPRAW

-- | Creates a new "fully-loaded" socket.
new :: Protocol p => p -> IO (Socket p)
new = newSocket domSP

-- [ ] [?] EBADF :: The provided socket is invalid.
--                  What does invalid mean here? Is this something that can
--                  be statically prevented?
-- [ ] [ ] EINTR :: Operation was interrupted by a signal. The socket is
--                  not fully closed yet. Operation can be re-started by
--                  calling nn_close() again.
--
--                  This absolutely must be caught by the user. It's an
--                  asynchronous exception that requires meaningful
--                  response.

-- | Closes an open socket. This handle must not be used again.
close :: Socket p -> IO ()
close s =
  do ret <- nn_close s
     if ret < 0 then nnGetErrno >>= error . show
                else return ()

-- [ ] [?]  EBADF          :: The provided socket is invalid.
-- [ ] [R]  EMFILE         :: Maximum number of active endpoints was reached.
-- [ ] [T?] EINVAL         :: The syntax of the supplied address is invalid.
--                This could be a user error---perhaps it'd be
--                better thrown as a `Maybe`? Or eliminated by
--                using types? For instance, the network-ip
--                package?
-- [X] [T] ENAMETOOLONG   :: The supplied address is too long.
-- [X] [T] EPROTONOSUPPORT :: The requested transport protocol is not supported.
-- [ ] [?] EADDRNOTAVAIL   :: The requested endpoint is not local.
--    How can this error arise? Someone would have to accidentally
--    provide a non-local address on the inproc:// or ipc://
--    schemes.
-- [ ] [T/] ENODEV         :: Address specifies a nonexistent interface.
--                Partially solvable by types (only allowing valid
--                interface types), but it also could be that the
--                IP fails to resolve. Can address parsing help this?
--
--                Could cause errors with IPC if the file cannot be accessed.
-- [ ] [ ] EADDRINUSE      :: The requested local endpoint is already in use.
--               This and not available just suggests that bind
--               should be an Either/Maybe function... tryBind, for
--               instance. They're both synchronous.
-- [ ] [U] ETERM           :: The library is terminating. 

-- | Bind to an address and begin listening.
bind :: Socket p -> Addr.Address -> IO Endpoint
bind s a =
  do ret <- Su.unsafeUseAsCString (Addr.toBS a) (nn_bind s)
     if ret < 0 then nnGetErrno >>= error . show
                else return (Endpoint ret)

-- | Connect to an address and begin sending.
connect :: Socket p -> Addr.Address -> IO Endpoint
connect s a = 
  do ret <- Su.unsafeUseAsCString (Addr.toBS a) (nn_connect s)
     if ret < 0 then nnGetErrno >>= error . show
                else return (Endpoint ret)

shutdown :: Socket p -> Endpoint -> IO ()
shutdown s e =
  do ret <- nn_shutdown s e
     if ret < 0 then nnGetErrno >>= error . show
                else return ()


-- CCalls
---------

-- Errors

foreign import ccall unsafe "nn.h nn_errno"
  nn_errno :: IO Errno

foreign import ccall unsafe "nn.h nn_strerror"
  nn_strerror :: Errno -> CString

-- Options

foreign import ccall unsafe "nn.h nn_setsockopt"
  nn_setsockopt :: Socket p -> CInt -> CInt -> Ptr a -> CSize -> IO CInt

-- Sockets

foreign import ccall unsafe "nn.h nn_socket"
  nn_socket :: Domain -> CInt -> IO CInt

foreign import ccall unsafe "nn.h nn_close"
  nn_close :: Socket p -> IO CInt

foreign import ccall unsafe "nn.h nn_bind"
  nn_bind :: Socket p -> CString -> IO CInt

foreign import ccall unsafe "nn.h nn_connect"
  nn_connect :: Socket p -> CString -> IO CInt

foreign import ccall unsafe "nn.h nn_shutdown"
  nn_shutdown :: Socket p -> Endpoint -> IO CInt
