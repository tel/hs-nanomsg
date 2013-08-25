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
-- Module      : Network.Nanomsg.C.Errors
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Nanomsg.C.Errors (

  -- * Error types

  -- Errors are semantically similar to POSIX errors, but Nanomsg is
  -- not guaranteed to be compatible with system POSIX error
  -- codes---it may redefine them for compatibility.
  --
  -- Furthermore, the system 'getErrno' may also not be
  -- compatible. Nanomsg exports its own 'nnGetErrno' for
  -- compatibility.
  
  Errno (..), nnGetErrno,
  
  -- ** Error codes

  eOK, eADDRINUSE, eADDRNOTAVAIL, eAFNOSUPPORT, eAGAIN, 
  eBADF, eCONNREFUSED, eFSM, eINPROGRESS, eINTR, eINVAL, 
  eMFILE, eNAMETOOLONG, eNETDOWN, eNOBUFS, eNODEV, eNOPROTOOPT, 
  eNOTSOCK, eNOTSUP, ePROTO, ePROTONOSUPPORT, eTERM, eTIMEDOUT,

  ) where

import Control.Exception

import Data.Data

import Foreign                 hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe        (unsafePerformIO)

import Network.Nanomsg.C.Syms

-- | Nanomsg has its own version of 'Errno'.
newtype Errno = Errno { errnoInt :: CInt } deriving (Eq, Storable)

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


-- Errors

foreign import ccall unsafe "nn.h nn_errno"
  nn_errno :: IO Errno

foreign import ccall unsafe "nn.h nn_strerror"
  nn_strerror :: Errno -> CString
