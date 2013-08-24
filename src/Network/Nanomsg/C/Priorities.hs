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
-- Module      : Network.Nanomsg.C.Priorities
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Nanomsg.C.Priorities (

  -- * Priorities

  -- Just a set of convenient constructors of thread priorities.

  Priority (..), incPrio, decPrio, prioLowest, prioHighest,

  prio1,  prio2,  prio3,  prio4,  prio5,  prio6,  prio7,  prio8,
  prio9,  prio10, prio11, prio12, prio13, prio14, prio15, prio16

  ) where

import Foreign (Storable)
import Foreign.C.Types

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
-- highest priority then nothing happens. This is used instead of an
-- 'Enum' instance to avoid throwing exceptions.
incPrio :: Priority -> Priority
incPrio p@(Priority n) | p <= prioHighest = Priority (succ n)
                       | otherwise        = prioHighest

-- | Decrease the priority by one notch. If it's already at the lowest
-- priority then nothing happens. This is used instead of an 'Enum'
-- instance to avoid throwing exceptions.
decPrio :: Priority -> Priority
decPrio p@(Priority n) | p <= prioLowest = Priority (pred n)
                       | otherwise       = prioLowest
