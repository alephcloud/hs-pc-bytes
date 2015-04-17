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
    , toBytesBase64
    , toBytesBase16
    , fromBytesBase64
    , fromBytesBase16
    ) where

import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Prelude hiding (splitAt, length, take, drop)

import PC.Bytes.Random
import PC.Bytes.Codec

-- | The class of ByteArrays for usage with
-- cryptographic ciphers.
--
-- Mininmal complete definition:
-- 'length', 'splitAt', 'toList', 'fromList', 'randomBytes'
--
class (Eq a, Ord a, Monoid a) => ByteArray a where

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

toBytesBase64 :: Bytes b => b -> BackendByteArray
toBytesBase64 = to64 . toBytes

fromBytesBase64 :: Bytes b => BackendByteArray -> Either String b
fromBytesBase64 b = from64 b >>= fromBytes

toBytesBase16 :: Bytes b => b -> BackendByteArray
toBytesBase16 = to16 . toBytes

fromBytesBase16 :: Bytes b => BackendByteArray -> Either String b
fromBytesBase16 b = from16 b >>= fromBytes

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
