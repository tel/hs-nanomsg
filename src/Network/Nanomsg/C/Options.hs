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
-- Module      : Network.Nanomsg.C.Options
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Network.Nanomsg.C.Options (

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

  ) where

import Foreign                 hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe        (unsafePerformIO)
import Network.Nanomsg.C.Syms

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


-- Options

foreign import ccall unsafe "nn.h nn_setsockopt"
  nn_setsockopt :: Socket p -> CInt -> CInt -> Ptr a -> CSize -> IO CInt
