-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module PC.Bytes.ByteArrayL
( ByteArrayL(..)
, BackendByteArrayL
, lengthL
, emptyL
, takeL
, takeEndL
, dropL
, dropEndL
, splitL
, concatL
, randomBytesL

-- ** Serialization
, BytesL(..)

-- ** type level numbers
, toInt
, type (<=)
) where

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.DeepSeq (NFData)
import Control.Monad

import Data.Monoid
import Data.Proxy

import Prelude hiding (splitAt, length, take, drop)

import PC.Bytes.ByteArray

import GHC.TypeLits

type BackendByteArrayL = ByteArrayL

-- -------------------------------------------------------------------------- --

newtype ByteArrayL (n :: Nat) = ByteArrayL BackendByteArray
    deriving (Eq, Ord, Show, NFData)

instance KnownNat n => Bytes (ByteArrayL n) where
    toBytes (ByteArrayL bytes) = bytes
    fromBytes a = ByteArrayL <$> (check =<< Right a)
        where
        e = toInt (Proxy :: Proxy n) :: Int
        check x = do
            let l = length x
            if l == toInt (Proxy :: Proxy n)
                then Right x
                else Left $ "wrong length: expected " ++ show e ++ " got " ++ show l

    -- Allows specialization elsewhere
    {-# INLINEABLE toBytes #-}
    {-# INLINEABLE fromBytes #-}

toInt :: forall v proxy a. (KnownNat v, Integral a) => proxy v -> a
toInt = fromIntegral . natVal

lengthL
    :: forall n . KnownNat n
    => ByteArrayL n
    -> Int
lengthL _ = toInt (Proxy :: Proxy n)
{-# INLINEABLE lengthL #-}

emptyL :: ByteArrayL 0
emptyL = ByteArrayL empty
{-# INLINABLE emptyL #-}

randomBytesL
    :: forall v . (KnownNat v)
    => IO (ByteArrayL v)
randomBytesL = ByteArrayL <$> randomBytes (toInt (Proxy :: Proxy v))
{-# INLINABLE randomBytesL #-}

dropL
    :: forall i m n proxy. (KnownNat i, i <= m, (m - i) ~ n)
    => proxy i
    -> ByteArrayL m
    -> ByteArrayL n
dropL i (ByteArrayL a) = ByteArrayL $ drop (toInt i) a
{-# INLINEABLE dropL #-}

dropEndL
    :: forall i m n proxy. (KnownNat i, i <= m, (m - i) ~ n)
    => proxy i
    -> ByteArrayL m
    -> ByteArrayL n
dropEndL i (ByteArrayL a) = ByteArrayL $ dropEnd (toInt i) a
{-# INLINEABLE dropEndL #-}

takeL
    :: forall m n. (KnownNat n, n <= m)
    => ByteArrayL m
    -> ByteArrayL n
takeL (ByteArrayL a) = ByteArrayL $ take (toInt (Proxy :: Proxy n)) a
{-# INLINEABLE takeL #-}

takeEndL
    :: forall m n. (KnownNat n, n <= m)
    => ByteArrayL m
    -> ByteArrayL n
takeEndL (ByteArrayL a) = ByteArrayL $ takeEnd (toInt (Proxy :: Proxy n)) a
{-# INLINEABLE takeEndL #-}

splitL
    :: forall m n o. (KnownNat n, n <= m, (m - n) ~ o)
    => ByteArrayL m
    -> (ByteArrayL n, ByteArrayL o)
splitL (ByteArrayL a) = (ByteArrayL *** ByteArrayL) $ splitAt (toInt (Proxy :: Proxy n)) a
{-# INLINABLE splitL #-}

concatL
    :: ((m + n) ~ x)
    => ByteArrayL m
    -> ByteArrayL n
    -> ByteArrayL x
concatL (ByteArrayL a) (ByteArrayL b) = ByteArrayL $ a `mappend` b
{-# INLINABLE concatL #-}

class (Bytes a) => BytesL a where
    type ByteLengthL a :: Nat
    toBytesL :: a -> ByteArrayL (ByteLengthL a)
    fromBytesL :: ByteArrayL (ByteLengthL a) -> Either String a

instance KnownNat n => BytesL (ByteArrayL n) where
    type ByteLengthL (ByteArrayL n) = n
    toBytesL = id
    fromBytesL = Right

    {-# INLINABLE toBytesL #-}
    {-# INLINABLE fromBytesL #-}

-- TODO:
{-# SPECIALIZE lengthL :: forall n . KnownNat n => ByteArrayL n -> Int #-}
{-# SPECIALIZE emptyL :: ByteArrayL 0 #-}
-- NOTE SPECIALIZE should be RULES now (vhanquez)
