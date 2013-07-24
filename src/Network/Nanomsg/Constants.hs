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

-- |
-- Module      : Network.Nanomsg.Constants
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

module Network.Nanomsg.Constants (

  -- * Error types

  -- Errors are semantically similar to POSIX errors, but Nanomsg is
  -- not guaranteed to be compatible with system POSIX error
  -- codes---it may redefine them for compatibility.
  --
  -- Furthermore, the system 'getErrno' may also not be
  -- compatible. Nanomsg exports its own 'nnGetErrno' for
  -- compatibility.
  
  Errno, nnGetErrno,
  
  -- ** Error codes

  eOK, eADDRINUSE, eADDRNOTAVAIL, eAFNOSUPPORT, eAGAIN, 
  eBADF, eCONNREFUSED, eFSM, eINPROGRESS, eINTR, eINVAL, 
  eMFILE, eNAMETOOLONG, eNETDOWN, eNOBUFS, eNODEV, eNOPROTOOPT, 
  eNOTSOCK, eNOTSUP, ePROTO, ePROTONOSUPPORT, eTERM, eTIMEDOUT,

  -- * Option types

  -- Option types are for use with @nn_setsockopt@. They use phantom
  -- types to ensure that the value set for that option is correct.

  Option,

  -- ** Option values

  optLINGER, optSNDBUF, optRCVBUF, optSNDTIMEO, optRCVTIMEO, 
  optRECONNECT_IVL, optRECONNECT_IVL_MAX, optSNDPRIO, optSNDFD, 
  optRCVFD, optDOMAIN, optPROTOCOL, optREQ_RESEND_IVL, optSUB_SUBSCRIBE, 
  optSUB_UNSUBSCRIBE, optSURVEYOR_DEADLINE, optTCP_NODELAY,

  -- * Protocol types

  -- Scalability protocols define the different operational modes of
  -- Nanomsg sockets. Each is its own datatype as the types appear
  -- later as parameters to other types.

  Protocol (),

  -- * Protocols
  Pair, Req, Rep, Pub, Sub,
  Surveyor, Respondent, Source, Sink,
  Push, Pull, Bus,

  -- * Transports

  -- Transports are mechanisms by which sockets connect to one another
  -- through @bind@ and @connect@.

  Transport, transTCP, transIPC, transINPROC,

  -- * Domains

  -- Domains are broad modes of operation of a socket. In particular,
  -- they let you choose to create a \"raw\" socket which is suitable
  -- for internal nodes in a topology.

  Domain, domSP, domSPRAW,

  -- * Priorities

  -- Just a set of convenient constructors of thread priorities.

  Priority, incPrio, decPrio, prioLowest, prioHighest,

  prio1,  prio2,  prio3,  prio4,  prio5,  prio6,  prio7,  prio8,
  prio9,  prio10, prio11, prio12, prio13, prio14, prio15, prio16,
  
  -- * The dynamic Nanomsg version
  version
  
  ) where

import Control.Applicative
import Control.Monad

import Foreign         hiding (unsafePerformIO, new)
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types

import qualified Data.Map as M

-- | Search through the Nanomsg symbol database via 'nn_symbol' and
-- create a mapping from names to constants. If the name isn't found,
-- raises an error as this should be a static guarantee.
symMap :: M.Map String CInt
symMap = m
  where
    m :: M.Map String CInt
    m = unsafePerformIO $ M.fromList <$> whileJustM [0..] get'
    
    get' n = alloca $ \ptr -> do
      cstr <- nn_symbol n ptr
      case cstr == nullPtr of
        True -> return Nothing
        False -> do
          val  <- peek ptr
          str  <- peekCString cstr
          return $ Just (str, val)

    whileJustM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
    whileJustM []       _   = return []
    whileJustM (x : xs) act = do
      res <- act x
      case res of
        Nothing -> return []
        Just v  -> (v:) `liftM` whileJustM xs act
{-# NOINLINE symMap #-}

getSym :: String -> CInt
getSym = (symMap M.!)
{-# NOINLINE getSym #-}

-- Version
----------

-- | The current version of Nanomsg.
version :: (Int, Int, Int)
version = ( fromIntegral $ getSym "NN_VERSION_MAJOR"
          , fromIntegral $ getSym "NN_VERSION_MINOR"
          , fromIntegral $ getSym "NN_VERSION_PATCH"
          )

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

-- | Kinds of options. Since options pass their values as @void *@
-- pointers, these are annotated with phantom types which must match
-- the passed option.
data Option a = Option
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
optLINGER            :: Option CInt
optLINGER            = Option (getSym "NN_LINGER") solLevel
                              
-- | Size of the send buffer, in bytes. To prevent blocking for
-- messages larger than the buffer, exactly one message may be
-- buffered in addition to the data in the send buffer. The type of
-- this option is int. Default value is 128kB.
optSNDBUF            :: Option CInt
optSNDBUF            = Option (getSym "NN_SNDBUF") solLevel

-- | Size of the receive buffer, in bytes. To prevent blocking for
-- messages larger than the buffer, exactly one message may be
-- buffered in addition to the data in the receive buffer. The type of
-- this option is int. Default value is 128kB.
optRCVBUF            :: Option CInt
optRCVBUF            = Option (getSym "NN_RCVBUF") solLevel

-- | The timeout for send operation on the socket, in milliseconds. If
-- message cannot be sent within the specified timeout, EAGAIN error
-- is returned. Negative value means infinite timeout. The type of the
-- option is int. Default value is -1.
optSNDTIMEO          :: Option CInt
optSNDTIMEO          = Option (getSym "NN_SNDTIMEO") solLevel

-- | The timeout for recv operation on the socket, in milliseconds. If
-- message cannot be received within the specified timeout, EAGAIN
-- error is returned. Negative value means infinite timeout. The type
-- of the option is int. Default value is -1.
optRCVTIMEO          :: Option CInt
optRCVTIMEO          = Option (getSym "NN_RCVTIMEO") solLevel

-- | For connection-based transports such as TCP, this option
-- specifies how long to wait, in milliseconds, when connection is
-- broken before trying to re-establish it. Note that actual reconnect
-- interval may be randomised to some extent to prevent severe
-- reconnection storms. The type of the option is int. Default value
-- is 100 (0.1 second).
optRECONNECT_IVL     :: Option CInt
optRECONNECT_IVL     = Option (getSym "NN_RECONNECT_IVL") solLevel

-- | This option is to be used only in addition to NN_RECONNECT_IVL
-- option. It specifies maximum reconnection interval. On each
-- reconnect attempt, the previous interval is doubled until
-- NN_RECONNECT_IVL_MAX is reached. Value of zero means that no
-- exponential backoff is performed and reconnect interval is based
-- only on NN_RECONNECT_IVL. If NN_RECONNECT_IVL_MAX is less than
-- NN_RECONNECT_IVL, it is ignored. The type of the option is
-- int. Default value is 0.
optRECONNECT_IVL_MAX :: Option CInt
optRECONNECT_IVL_MAX = Option (getSym "NN_RECONNECT_IVL_MAX") solLevel

-- | Retrieves outbound priority currently set on the socket. This
-- option has no effect on socket types that send messages to all the
-- peers. However, if the socket type sends each message to a single
-- peer (or a limited set of peers), peers with high priority take
-- precedence over peers with low priority. The type of the option is
-- int. Highest priority is 1, lowest priority is 16. Default value is
-- 8.
optSNDPRIO           :: Option Priority
optSNDPRIO           = Option (getSym "NN_SNDPRIO") solLevel

-- | Retrieves a file descriptor that is readable when a message can
-- be sent to the socket. The descriptor should be used only for
-- polling and never read from or written to. The type of the option
-- is same as the type of file descriptor on the platform. That is,
-- int on POSIX-complaint platforms and SOCKET on Windows. The
-- descriptor becomes invalid and should not be used any more once the
-- socket is closed. This socket option is not available for
-- unidirectional recv-only socket types.
optSNDFD             :: Option Fd
optSNDFD             = Option (getSym "NN_SNDFD") solLevel

-- | Retrieves a file descriptor that is readable when a message can
-- be received from the socket. The descriptor should be used only for
-- polling and never read from or written to. The type of the option
-- is same as the type of file descriptor on the platform. That is,
-- int on POSIX-complaint platforms and SOCKET on Windows. The
-- descriptor becomes invalid and should not be used any more once the
-- socket is closed. This socket option is not available for
-- unidirectional send-only socket types.
optRCVFD             :: Option Fd
optRCVFD             = Option (getSym "NN_RCVFD") solLevel

-- | Returns the domain constant as it was passed to nn_socket().
optDOMAIN            :: Option Domain
optDOMAIN            = Option (getSym "NN_DOMAIN") solLevel

-- | Returns the protocol constant as it was passed to nn_socket().
optPROTOCOL          :: Protocol a => Option a
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
optREQ_RESEND_IVL :: Option CInt
optREQ_RESEND_IVL = Option (getSym "NN_REQ_RESEND_IVL")
                           (proValue Req)

-- | Defined on full SUB socket. Subscribes for a particular
-- topic. Type of the option is string.
optSUB_SUBSCRIBE :: Option String
optSUB_SUBSCRIBE = Option (getSym "NN_SUB_SUBSCRIBE")
                          (proValue Sub)

-- | Defined on full SUB socket. Unsubscribes from a particular
-- topic. Type of the option is string.
optSUB_UNSUBSCRIBE :: Option String
optSUB_UNSUBSCRIBE = Option (getSym "NN_SUB_UNSUBSCRIBE")
                            (proValue Sub)

-- | Specifies how long to wait for responses to the survey. Once the
-- deadline expires, receive function will return ETIMEDOUT error and
-- all subsequent responses to the survey will be silently
-- dropped. The deadline is measured in milliseconds. Option type is
-- int. Default value is 1000 (1 second).
optSURVEYOR_DEADLINE :: Option CInt
optSURVEYOR_DEADLINE = Option (getSym "NN_SURVEYOR_DEADLINE")
                              (proValue Surveyor)


-- Transport-level Options
--------------------------

-- | This option, when set to 1, disables Nagle’s algorithm. It also
-- disables delaying of TCP acknowledgments. Using this option
-- improves latency at the expense of throughput. Type of this option
-- is int. Default value is 0.
optTCP_NODELAY :: Option Bool
optTCP_NODELAY = Option (getSym "NN_TCP_NODELAY")
                        (getTransport transTCP)

instance Show (Option a) where
  show (Option i l)
    | l == solLevel = case () of
      () 
        | i == getSym "NN_LINGER"             -> "Socket Option {Linger}"
        | i == getSym "NN_SNDBUF"             -> "Socket Option {Send Buffer Size}"
        | i == getSym "NN_RCVBUF"             -> "Socket Option {Receive Buffer Size}"
        | i == getSym "NN_SNDTIMEO"           -> "Socket Option {Send Timeout}"
        | i == getSym "NN_RCVTIMEO"           -> "Socket Option {Receive Timeout}"
        | i == getSym "NN_RECONNECT_IVL"      -> "Socket Option {Reconnect IVL}"
        | i == getSym "NN_RECONNECT_IVL_MAX"  -> "Socket Option {Reconnect IVL Maximum}"
        | i == getSym "NN_SNDPRIO"            -> "Socket Option {Send Priority}"
        | i == getSym "NN_SNDFD"              -> "Socket Option {Send File Descriptor}"
        | i == getSym "NN_RCVFD"              -> "Socket Option {Receive File Descriptor}"
        | i == getSym "NN_DOMAIN"             -> "Socket Option {Domain}"
        | i == getSym "NN_PROTOCOL"           -> "Socket Option {Protocol}"
        | i == getSym "NN_IPV4ONLY"           -> "Socket Option {IPv4 Only}"
        | otherwise                           -> "Option {unknown}"

    | l == proValue Req = case () of
      ()
        | i == getSym "NN_REQ_RESEND_IVL"     -> "Protocol Option {Req/Resend IVL}"
        | otherwise                           -> "Option {unknown}"

    | l == proValue Sub = case () of
      ()
        | i == getSym "NN_SUB_SUBSCRIBE"      -> "Protocol Option {Sub/Subscribe}"
        | i == getSym "NN_SUB_UNSUBSCRIBE"    -> "Protocol Option {Sub/Unsubscribe}"
        | otherwise                           -> "Option {unknown}"

    | l == proValue Surveyor = case () of
      ()
        | i == getSym "NN_SURVEYOR_DEADLINE"  -> "Protocol Option {Surveyor/Deadline}"  
        | otherwise                           -> "Option {unknown}"

    | l == getTransport transTCP = case () of
      ()
        | i == getSym "NN_TCP_NODELAY"        -> "Transport Option {TCP/No delay}"
        | otherwise                           -> "Option {unknown}"

    | otherwise                               =  "Option {unknown}"

-- Protocols
------------

-- | A scalability protocol.
class Protocol p where
  proValue :: p -> CInt

-- | Socket for communication with exactly one peer. Each party can
-- send messages at any time. If the peer is not available or send
-- buffer is full subsequent calls to nn_send(3) will block until it’s
-- possible to send the message.
data Pair = Pair deriving (Show)

instance Protocol Pair where
  proValue _ = getSym "NN_PAIR"

-- | Used to implement the client application that sends requests and
-- receives replies.
data Req = Req deriving (Show)

instance Protocol Req where
  proValue _ = getSym "NN_REQ"

-- | Used to implement the stateless worker that receives requests and
-- sends replies.
data Rep = Rep deriving (Show)

instance Protocol Rep where
  proValue _ = getSym "NN_REP"

-- | This socket is used to distribute messages to multiple
-- destinations. Receive operation is not defined.
data Pub = Pub deriving (Show)

instance Protocol Pub where
  proValue _ = getSym "NN_PUB"

-- | Receives messages from the publisher. Only messages that the
-- socket is subscribed to are received. When the socket is created
-- there are no subscriptions and thus no messages will be
-- received. Send operation is not defined on this socket. The socket
-- can be connected to at most one peer.
data Sub = Sub deriving (Show)

instance Protocol Sub where
  proValue _ = getSym "NN_SUB"

-- | Used to send the survey. The survey is delivered to all the
-- connected respondents. Once the query is sent, the socket can be
-- used to receive the responses. When the survey deadline expires,
-- receive will return ETIMEDOUT error.
data Surveyor = Surveyor deriving (Show)

instance Protocol Surveyor where
  proValue _ = getSym "NN_SURVEYOR"

-- | Use to respond to the survey. Survey is received using receive
-- function, response is sent using send function. This socket can be
-- connected to at most one peer.
data Respondent = Respondent deriving (Show)

instance Protocol Respondent where
  proValue _ = getSym "NN_RESPONDENT"

-- | Allows to send messages to the central sink. Receive operation is
-- not implemented on this socket type. This socket can be connected
-- to at most one peer.
data Source = Source deriving (Show)

instance Protocol Source where
  proValue _ = getSym "NN_SOURCE"

-- | Receives the messages from multiple sources. Send operation is
-- not defined on this socket type.
data Sink = Sink deriving (Show)

instance Protocol Sink where
  proValue _ = getSym "NN_SINK"

-- | This socket is used to send messages to the cluster of
-- load-balanced nodes. Receive operation is not implemented on this
-- socket type.
data Push = Push deriving (Show)

instance Protocol Push where
  proValue _ = getSym "NN_PUSH"

-- | This socket is used to implement a node within a load-balanced
-- cluster. It can be used to receive messages. Send operation is not
-- implemented on this socket type. This socket can be connected to at
-- most one peer.
data Pull = Pull deriving (Show)

instance Protocol Pull where
  proValue _ = getSym "NN_PULL"

-- | Sent messages are distributed to all nodes in the
-- topology. Incoming messages from all other nodes in the topology
-- are fair-queued in the socket.
data Bus = Bus deriving (Show)

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

-- CCalls
---------

foreign import ccall unsafe "nn.h nn_symbol"
  nn_symbol :: CInt -> Ptr CInt -> IO (Ptr CChar)

foreign import ccall safe "nn.h nn_errno"
  nn_errno :: IO Errno

foreign import ccall safe "nn.h nn_strerror"
  nn_strerror :: Errno -> CString
