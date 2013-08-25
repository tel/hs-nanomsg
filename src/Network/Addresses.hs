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
-- Module      : Network.Addresses
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : Apache 2.0
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Module modeling IPv4 and IPv6 addresses.

module Network.Addresses where

import Control.Applicative

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as SL
import Data.ByteString.Builder (toLazyByteString, char7)
import Data.ByteString.Builder.Prim (word8Dec, word16Hex, primBounded)
import Data.Attoparsec.Char8
import Data.Monoid
import Data.List
import Data.String
import Data.Word
import Data.Char

import Network.Nanomsg.Util

data IPv4 = IPv4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
            deriving (Eq, Ord)

instance Show IPv4 where show ip = "IPv4 " ++ S8.unpack (ser ip)

instance Serial IPv4 where
  ser (IPv4 a b c d) =
    SL.toStrict
    . toLazyByteString
    . mconcat
    . intersperse (char7 '.')
    . map (primBounded word8Dec)
    $ [a, b, c, d]

ipv4Parser :: Parser IPv4
ipv4Parser =
  IPv4 <$> (decWord8 <* char '.')
       <*> (decWord8 <* char '.')
       <*> (decWord8 <* char '.')
       <*> decWord8
  where decWord8 =
          do x <- decimal
             if (x >= 0 && x <= 128)
               then return x
               else fail "invalid IPv4 digit"         

instance IsString IPv4 where
  fromString s = let Right ip = parseOnly ipv4Parser (S8.pack s) in ip

-- decimal
-- hexadecimal

data IPv6 = IPv6 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
                 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
                 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
                 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
            deriving (Eq, Ord)

instance Show IPv6 where show ip = "IPv6 " ++ S8.unpack (ser ip)

-- | Doesn't do double-colon compression.
instance Serial IPv6 where
  ser (IPv6 a b c d e f g h) =
    SL.toStrict
    . toLazyByteString
    . mconcat
    . intersperse (char7 ':')
    . map (primBounded word16Hex)
    $ [a, b, c, d, e, f, g, h]

-- | Parses a IPv6 address. Trims each word to the right and doesn't
-- handle the repeated 0 shortening.
-- 
-- TODO: Handle repeated zeros properly for addresses like "::1".
-- 
--   parseOnly ipv6Parser "0:0:0:0:0:0:1234567890abcdef:0"
--     == Right 0:0:0:0:0:0:cdef:0
-- 
ipv6Parser :: Parser IPv6
ipv6Parser =
  IPv6 <$> (hexadecimal <* char ':')
       <*> (hexadecimal <* char ':')
       <*> (hexadecimal <* char ':')
       <*> (hexadecimal <* char ':')
       <*> (hexadecimal <* char ':')
       <*> (hexadecimal <* char ':')
       <*> (hexadecimal <* char ':')  
       <*> hexadecimal

instance IsString IPv6 where
  fromString s = let Right ip = parseOnly ipv6Parser (S8.pack s) in ip


data IP = V4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
             {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
        | V6 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
             {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
             {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
             {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
        deriving (Eq, Ord)

forgetIPv4 :: IPv4 -> IP
forgetIPv4 (IPv4 a b c d) = V4 a b c d

forgetIPv6 :: IPv6 -> IP
forgetIPv6 (IPv6 a b c d e f g h) = V6 a b c d e f g h

instance Serial IP where
  ser (V4 a b c d) = ser (IPv4 a b c d)
  ser (V6 a b c d e f g h) = ser (IPv6 a b c d e f g h)

instance Show IP where show ip = "IP " ++ S8.unpack (ser ip)

instance IsString IP where
  fromString s = let Right ip = parseOnly parser (S8.pack s) in ip
    where parser =     (forgetIPv4 <$> ipv4Parser)
                   <|> (forgetIPv6 <$> ipv6Parser)
