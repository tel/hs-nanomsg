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
-- Module      : Network.Nanomsg.C.Domains
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Nanomsg.C.Domains (


  -- * Domains

  -- Domains are broad modes of operation of a socket. In particular,
  -- they let you choose to create a \"raw\" socket which is suitable
  -- for internal nodes in a topology.

  Domain (..), domSP, domSPRAW

  ) where

import Foreign (Storable)
import Foreign.C.Types
import Network.Nanomsg.C.Syms

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
