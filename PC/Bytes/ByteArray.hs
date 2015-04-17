-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module PC.Bytes.ByteArray
( ByteArray(..)
, Bytes(..)
, FromBytesSafe(..)
, BackendByteArray
, module PC.Bytes.Codec
) where

import Control.Applicative hiding (empty)
import Control.Monad.IO.Class

import Crypto.Random

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.List as L

import Prelude hiding (splitAt, length, take, drop)

import PC.Bytes.Codec
import PC.Bytes.Random

-- | The class of ByteArrays for usage with
-- cryptographic ciphers.
--
-- Mininmal complete definition:
-- 'length', 'splitAt', 'toList', 'fromList', 'randomBytes'
--
class (Eq a, Ord a, Monoid a, Code64 a, Code16 a) => ByteArray a where

    length :: a -> Int
    splitAt :: Int -> a -> (a, a)
    randomBytes :: MonadIO mu => Int -> mu a
    toList :: a -> [Word8]
    fromList :: [Word8] -> a

    splitAtEnd :: Int -> a -> (a, a)
    take :: Int -> a -> a
    takeEnd :: Int -> a -> a
    drop :: Int -> a -> a
    dropEnd :: Int -> a -> a
    empty :: a

    -- Default implementations
    splitAtEnd i a = splitAt (length a - i) a
    take i = fst . splitAt i
    takeEnd i = snd . splitAtEnd i
    drop i = snd . splitAt i
    dropEnd i = fst . splitAtEnd i
    empty = mempty

    {-# INLINABLE splitAtEnd #-}
    {-# INLINABLE take #-}
    {-# INLINEABLE takeEnd #-}
    {-# INLINEABLE drop #-}
    {-# INLINEABLE dropEnd #-}

-- | The class of types that can be serialized to ByteArrays.
--
-- Note that 'ByteArray' implementation is fixed. In order
-- to create different implementation of 'ByteArray'
-- a newtype wrapper can be used.
--
-- Minimal complete defintion:
-- 'toBytes', 'fromBytes'
--
class Bytes a where
    toBytes :: a -> BackendByteArray
    fromBytes :: BackendByteArray -> Either String a

-- | Object that can be converted to a type from a bytestring without failing.
--
-- this is useful for instances of objects that are just newtype of bytestring
-- and don't have any length or content constraint (type-wise). e.g. Password
class FromBytesSafe a where
    fromBytesSafe :: BackendByteArray -> a

type BackendByteArray = ByteString

instance ByteArray B.ByteString where

    length = B.length
    take = B.take
    drop = B.drop
    splitAt = B.splitAt
    fromList = B.pack
    toList = B.unpack

    {-# INLINABLE length #-}
    {-# INLINABLE take #-}
    {-# INLINABLE drop #-}
    {-# INLINABLE splitAt #-}
    {-# INLINABLE fromList #-}
    {-# INLINABLE toList #-}

    randomBytes i = liftIO $ getRandom i

instance Bytes B.ByteString where
    toBytes = id
    fromBytes = Right

    {-# INLINABLE toBytes #-}
    {-# INLINABLE fromBytes #-}

instance Bytes T.Text where
    toBytes = T.encodeUtf8
    fromBytes = either (Left . show) Right . T.decodeUtf8'

-- -------------------------------------------------------------------------- --
-- * Codec Instances

-- -------------------------------------------------------------------------- --
-- ** Base64 serialization

instance Code64 B.ByteString where
    to64 = B8.unpack . urlEncode64
    from64 = urlDecode64 . B8.pack

-- -------------------------------------------------------------------------- --
-- ** Hex serialization

instance Code16 B.ByteString where
    to16 = B8.unpack . B16.encode
    from16 = Right . fst . B16.decode . B8.pack

-- -------------------------------------------------------------------------- --
-- ** Utils

urlEncode64 :: B.ByteString -> B.ByteString
urlEncode64 = fst . B8.spanEnd (== '=') . B64.encode

urlDecode64 :: B.ByteString -> Either String B.ByteString
urlDecode64 s = let l = B.length s
                    x = l `mod` 4
                in  B64.decode (s `mappend` B8.replicate (4 - if x == 0 then 4 else x) '=')
