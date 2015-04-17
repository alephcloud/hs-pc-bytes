-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module PC.Bytes.Utils
( unsafeFromBytes
, unsafeFromBytesL
, padLeft
, splitHalf
, splitHalfL
, HalfF
, HalfC
, (%)

-- * Binary Parser
, Parser(..)
, (<?>)
, parse
, parse'
, eof
, isEof
, pEither
, pAssert
, pListL
, pTake
, pTakeAll
, pTakeAllBytes
, pRemaining
, pTakeExcept
, pTakeExceptBytes
, pTakeBytesL
, pTakeBytes
) where

import Control.Applicative hiding (empty)
import qualified Control.Applicative as AP
import Control.Arrow hiding (left, right)

import Data.Proxy
import Data.Word (Word8)
import Data.Monoid

import GHC.TypeLits

import Prelude hiding (splitAt, length)

import PC.Bytes.ByteArray
import PC.Bytes.ByteArrayL

-- -------------------------------------------------------------------------- --
-- * Utils

-- | Use this method only for 'fromBytes' conversions that are fully generic,
-- i.e. for unconstraint newtype wrappers or in places with strong
-- local code invariants. For instance:
--
-- > byteArray <- randomBytes 100
-- > let a = take 10 byteArray
-- > let typeWithExactly10bytesArray = unsafeFromBytes a
--
-- The only thing that possibly can go wrong here is a length mismatch
-- (which we currently don't check). Hence, if something goes wrong here
-- it's a bug in the code and throwing an asynchronous exception is fine.
--
-- FIXME Make this a type class and avoid the partial function!
--
unsafeFromBytes :: forall a . Bytes a => BackendByteArray -> a
unsafeFromBytes = either (\e -> error $ "Failed to interpret bitArray. This is a bug in the code: " ++ e) id . fromBytes

unsafeFromBytesL :: forall a . (BytesL a) => ByteArrayL (ByteLengthL a) -> a
unsafeFromBytesL = either error id . fromBytesL

-- | pad a ByteArray on the left
--
-- > length (padLeft a i b) == i
--
padLeft :: ByteArray a => Word8 -> Int -> a -> a
padLeft a i b
    | (length b) < i = fromList (replicate (i - length b) a) `mappend` b
    | otherwise = b

(%)
    :: ((n + m) ~ o)
    => BackendByteArrayL n
    -> BackendByteArrayL m
    -> BackendByteArrayL o
(%) = concatL

-- | For odd input length the first component of the result
-- is one byte shorter than the second component.
--
splitHalf
    :: BackendByteArray
    -> (BackendByteArray, BackendByteArray)
splitHalf s = splitAt (length s `div` 2) s

-- | For odd input length the first component of the result
-- is one byte shorter than the second component.
--
splitHalfL
    -- :: forall n m0 m1 . (m0 <= n, m1 ~ (n - m0), (m0 + m1) ~ n, (m0 ~ HalfF n, m1 ~ HalfC n)
    :: forall n m0 m1 . (KnownNat m0, m0 <= n, m1 ~ (n - m0), (m0 + m1) ~ n, HalfF n ~ m0, HalfC n ~ m1)
    => BackendByteArrayL n
    -> (BackendByteArrayL m0, BackendByteArrayL m1)
splitHalfL n = splitL n

-- | @HalfF n == floor (n/2)@
--
-- Complexity: @O(n)@
--
type family HalfF_ (n :: Nat) :: Nat where
    HalfF_ 0 = 0
    HalfF_ 1 = 0
    HalfF_ n = HalfF_ (n - 2) + 1

-- | @HalfF n == floor (n/2)@
--
-- Complexity: @O(log^2 n)@
--
type HalfF (n :: Nat) = HalfF0 n 1

type family HalfF0 (n :: Nat) (l :: Nat) :: Nat where
    HalfF0 0 l = 0
    HalfF0 1 l = 0
    HalfF0 n l = HalfF1 (CmpNat n (4 * l)) n l

type family HalfF1 (x :: Ordering) (n :: Nat) (l :: Nat) :: Nat where
    HalfF1 EQ n u = 2 * u
    HalfF1 GT n u = HalfF0 n (2 * u)
    HalfF1 LT n u = u + HalfF0 (n - (2 * u)) 1

-- | @HalfC_ n == ceiling (n/2)@
--
-- Complexity: @O(n)@
--
type family HalfC_ (n :: Nat) :: Nat where
    HalfC_ 0 = 0
    HalfC_ 1 = 1
    HalfC_ n = HalfC_ (n - 2) + 1

-- | @HalfC n == ceiling (n/2)@
--
-- Complexity: @O(log^2 n)@
--
type HalfC (n :: Nat) = HalfC0 n 1

type family HalfC0 (n :: Nat) (l :: Nat) :: Nat where
    HalfC0 0 l = 0
    HalfC0 1 l = 1
    HalfC0 n l = HalfC1 (CmpNat n (4 * l)) n l

type family HalfC1 (x :: Ordering) (n :: Nat) (l :: Nat) :: Nat where
    HalfC1 EQ n u = 2 * u
    HalfC1 GT n u = HalfC0 n (2 * u)
    HalfC1 LT n u = u + HalfC0 (n - (2 * u)) 1

-- -------------------------------------------------------------------------- --
-- * A simple (yet) non-backtracking deterministic parser for 'ByteArray's

-- | A simple non-backtracking deterministic parser simple parser that is
-- generic with respect to the underlying ByteArray
--
-- The type-parameter is the underlying ByteArray implementation.
--
newtype Parser a = Parser { unBAP :: BackendByteArray -> (Either String a, BackendByteArray) }

pEither :: (a -> Either String b) -> Parser a -> Parser b
pEither f p = Parser $ \(a :: BackendByteArray) -> case (unBAP p) a of
    (Right r, a') -> case f r of
        Right r' -> (Right r', a')
        Left e -> (Left e, a)
    (Left e, _) -> (Left e, a)

pAssert :: String -> (a -> Bool) -> Parser a -> Parser a
pAssert msg f = pEither $ \a -> if f a then Right a else Left msg

-- | Consumes remaining input into a list of
-- values parsed by the given parser.
--
pListL :: ByteArray BackendByteArray => Parser a -> Parser [a]
pListL p = (eof *> pure []) <|> ((:) <$> p <*> pListL p) <?> "pListL"

pTake :: Int -> Parser BackendByteArray
pTake i = Parser $ \a -> if i <= length a
    then first Right $ splitAt i a
    else (Left "input too short", a)

pTakeBytes :: Bytes a => Int -> Parser a
pTakeBytes i = pEither fromBytes (pTake i)

pTakeBytesL :: forall a . (KnownNat (ByteLengthL a), BytesL a) => Parser a
pTakeBytesL = pEither fromBytesL (pTakeL :: Parser (ByteArrayL (ByteLengthL a)))

pTakeL :: forall n . KnownNat n => Parser (ByteArrayL n)
pTakeL = pEither fromBytes $ pTake (toInt (Proxy :: Proxy n))

pTakeExcept :: Int -> Parser BackendByteArray
pTakeExcept i =  Parser $ \a -> if i <= length a
    then first Right $ splitAtEnd i a
    else (Left "input too short", a)

pTakeExceptBytes :: (Bytes a) => Int -> Parser a
pTakeExceptBytes i = pEither fromBytes (pTakeExcept i)

-- | This parser returns the length of the remaining input.
-- It does not consume any bytes.
--
-- Depending on the implementation of 'BackendByteArray' this
-- may not always terminate.
--
pRemaining :: Parser Int
pRemaining = Parser $ \a -> (Right $ length a, a)

-- | This parser returns all remaining input.
--
-- Depending on the implementation of 'ByteArray' this
-- may not always terminate.
--
pTakeAll :: Parser BackendByteArray
pTakeAll = Parser $ \a -> (Right a, empty)

-- | This parser applies 'fromBytes' on all remaining input
--
-- Depending on the implementation of 'ByteArray' this
-- may not always terminate.
--
pTakeAllBytes :: Bytes a => Parser a
pTakeAllBytes = pEither fromBytes pTakeAll

(<?>) :: Parser a -> String -> Parser a
(<?>) p s = Parser $ \(a :: BackendByteArray) -> case (unBAP p) a of
    (Left e, _) -> (Left ("in " ++ s ++ ": " ++ e), a)
    x -> x

infixl 3 <?>

-- | This parser consumes no input. It never fails.
--
isEof :: Parser Bool
isEof = Parser $ \case
    a | length a == 0 -> (Right True, a)
      | otherwise -> (Right False, a)

eof :: Parser ()
eof = Parser $ \case
    a| length a == 0 -> (Right (), a)
     | otherwise -> (Left ("eof: remaining input: " ++ to16 a), a)

parse :: Parser a -> BackendByteArray -> Either String a
parse = parse' ""

parse' :: String -> Parser a -> BackendByteArray -> Either String a
parse' s (Parser p) a = case p a of
    (Right r, a') -> if length a' == 0
        then Right r
        else Left $ "failed to consume all input while parsing" ++ ss ++ "; remaining bytes are: " ++ to16 a'
    (Left e, a') -> Left $ "failed to parse" ++ ss ++ ": " ++ e ++ ". remaining bytes are: " ++ to16 a'
  where
    ss = if s == "" then "" else " " ++ s

instance Functor Parser where
    fmap f (Parser p) = Parser $ first (fmap f) . p

instance Applicative Parser where
    pure x = Parser $ \a -> (Right x, a)
    (Parser p0) <*> (Parser p1) = Parser $ \a ->
        case p0 a of
            (Left l, _) -> (Left l, a)
            (Right r, a') -> first (fmap r) $ (p1 a')

instance Alternative Parser where
    empty = Parser $ \s -> (Left "empty", s)
    (<|>) a b = Parser $ \x -> case unBAP a x of
        r@(Right {}, _) -> r
        (Left s, _) -> case unBAP b x of
            r'@(Right {}, _) -> r'
            (Left s', t) -> (Left ("[" ++ s ++ "," ++ s' ++ "]"), t)

instance Monad Parser where
    return  = pure
    a >>= b = Parser $ \x ->
        case unBAP a x of
            (Right r, bs') -> unBAP (b r) bs'
            (Left v, bs')  -> (Left v, bs')
