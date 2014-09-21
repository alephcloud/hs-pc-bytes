-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

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

import Data.Monoid.Unicode
import Data.Proxy
import Data.Word (Word8)

import GHC.TypeLits

import Prelude hiding (splitAt, length)
import Prelude.Unicode

import PC.Bytes.ByteArray
import PC.Bytes.ByteArrayL

-- -------------------------------------------------------------------------- --
-- * Utils

-- | Use this method only for 'fromBytes' conversions that are fully generic,
-- i.e. for unconstraint newtype wrappers or in places with strong
-- local code invariants. For instance:
--
-- > byteArray ← randomBytes 100
-- > let a = take 10 byteArray
-- > let typeWithExactly10bytesArray = unsafeFromBytes a
--
-- The only thing that possibly can go wrong here is a length mismatch
-- (which we currently don't check). Hence, if something goes wrong here
-- it's a bug in the code and throwing an asynchronous exception is fine.
--
-- FIXME Make this a type class and avoid the partial function!
--
unsafeFromBytes ∷ ∀ α . Bytes α ⇒ ByteArrayImpl α → α
unsafeFromBytes = either (\e → error $ "Failed to interpret bitArray. This is a bug in the code: " ⊕ e) id ∘ fromBytes

unsafeFromBytesL ∷ ∀ α . (BytesL α) ⇒ ByteArrayL (ByteArrayImpl α) (ByteLengthL α) → α
unsafeFromBytesL = either error id ∘ fromBytesL

-- | pad a ByteArray on the left
--
-- > length (padLeft a i b) ≡ i
--
padLeft ∷ ByteArray α ⇒ Word8 → Int → α → α
padLeft a i b
    | (length b) < i = fromList (replicate (i - length b) a) ⊕ b
    | otherwise = b

(%)
    ∷ ((n + m) ~ o)
    ⇒ BackendByteArrayL n
    → BackendByteArrayL m
    → BackendByteArrayL o
(%) = concatL

-- | For odd input length the first component of the result
-- is one byte shorter than the second component.
--
splitHalf
    ∷ BackendByteArray
    → (BackendByteArray, BackendByteArray)
splitHalf s = splitAt (length s `div` 2) s

-- | For odd input length the first component of the result
-- is one byte shorter than the second component.
--
splitHalfL
    -- ∷ ∀ n m0 m1 . (m0 ≤ n, m1 ~ (n - m0), (m0 + m1) ~ n, (m0 ~ HalfF n, m1 ~ HalfC n)
    ∷ ∀ n m0 m1 . (KnownNat m0, m0 ≤ n, m1 ~ (n - m0), (m0 + m1) ~ n, HalfF n ~ m0, HalfC n ~ m1)
    ⇒ BackendByteArrayL n
    → (BackendByteArrayL m0, BackendByteArrayL m1)
splitHalfL n = splitL n

-- | @HalfF n ≡ floor (n/2)@
--
-- Complexity: @O(n)@
--
type family HalfF_ (n ∷ Nat) ∷ Nat where
    HalfF_ 0 = 0
    HalfF_ 1 = 0
    HalfF_ n = HalfF_ (n - 2) + 1

-- | @HalfF n ≡ floor (n/2)@
--
-- Complexity: @O(log^2 n)@
--
type HalfF (n ∷ Nat) = HalfF0 n 1

type family HalfF0 (n ∷ Nat) (l ∷ Nat) ∷ Nat where
    HalfF0 0 l = 0
    HalfF0 1 l = 0
    HalfF0 n l = HalfF1 (CmpNat n (4 * l)) n l

type family HalfF1 (x ∷ Ordering) (n ∷ Nat) (l ∷ Nat) ∷ Nat where
    HalfF1 EQ n u = 2 * u
    HalfF1 GT n u = HalfF0 n (2 * u)
    HalfF1 LT n u = u + HalfF0 (n - (2 * u)) 1

-- | @HalfC_ n ≡ ceiling (n/2)@
--
-- Complexity: @O(n)@
--
type family HalfC_ (n ∷ Nat) ∷ Nat where
    HalfC_ 0 = 0
    HalfC_ 1 = 1
    HalfC_ n = HalfC_ (n - 2) + 1

-- | @HalfC n ≡ ceiling (n/2)@
--
-- Complexity: @O(log^2 n)@
--
type HalfC (n ∷ Nat) = HalfC0 n 1

type family HalfC0 (n ∷ Nat) (l ∷ Nat) ∷ Nat where
    HalfC0 0 l = 0
    HalfC0 1 l = 1
    HalfC0 n l = HalfC1 (CmpNat n (4 * l)) n l

type family HalfC1 (x ∷ Ordering) (n ∷ Nat) (l ∷ Nat) ∷ Nat where
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
newtype Parser π α = Parser { unBAP ∷ π → (Either String α, π) }

pEither ∷ ∀ π α β . ByteArray π ⇒ (α → Either String β) → Parser π α → Parser π β
pEither f p = Parser $ \(a ∷ π) → case (unBAP p) a of
    (Right r, a') → case f r of
        Right r' → (Right r', a')
        Left e → (Left e, a)
    (Left e, _) → (Left e, a)

pAssert ∷ ByteArray π ⇒ String → (α → Bool) → Parser π α → Parser π α
pAssert msg f = pEither $ \a → if f a then Right a else Left msg

-- | Consumes remaining input into a list of
-- values parsed by the given parser.
--
pListL ∷ ByteArray π ⇒ Parser π α → Parser π [α]
pListL p = (eof *> pure []) <|> ((:) <$> p <*> pListL p) <?> "pListL"

pTake ∷ ByteArray α ⇒ Int → Parser α α
pTake i = Parser $ \a → if i ≤ length a
    then first Right $ splitAt i a
    else (Left "input too short", a)

pTakeBytes ∷ (Bytes α) ⇒ Int → Parser (ByteArrayImpl α) α
pTakeBytes i = pEither fromBytes (pTake i)

pTakeBytesL ∷ ∀ α . (KnownNat (ByteLengthL α), BytesL α) ⇒ Parser (ByteArrayImpl α) α
pTakeBytesL = pEither fromBytesL (pTakeL ∷ Parser (ByteArrayImpl α) (ByteArrayL (ByteArrayImpl α) (ByteLengthL α)))

pTakeL ∷ ∀ α n . (KnownNat n, ByteArray α) ⇒ Parser α (ByteArrayL α n)
pTakeL = pEither fromBytes $ pTake (toInt (Proxy ∷ Proxy n))

pTakeExcept ∷ ByteArray π ⇒ Int → Parser π π
pTakeExcept i =  Parser $ \a → if i ≤ length a
    then first Right $ splitAtEnd i a
    else (Left "input too short", a)

pTakeExceptBytes ∷ (Bytes α) ⇒ Int → Parser (ByteArrayImpl α) α
pTakeExceptBytes i = pEither fromBytes (pTakeExcept i)

-- | This parser returns the length of the remaining input.
-- It does not consume any bytes.
--
-- Depending on the implementation of 'BackendByteArray' this
-- may not always terminate.
--
pRemaining ∷ ∀ π . ByteArray π ⇒ Parser π Int
pRemaining = Parser $ \a → (Right $ length a, a)

-- | This parser returns all remaining input.
--
-- Depending on the implementation of 'ByteArray' this
-- may not always terminate.
--
pTakeAll ∷ ByteArray α ⇒ Parser α α
pTakeAll = Parser $ \a → (Right a, empty)

-- | This parser applies 'fromBytes' on all remaining input
--
-- Depending on the implementation of 'ByteArray' this
-- may not always terminate.
--
pTakeAllBytes ∷ (Bytes α) ⇒ Parser (ByteArrayImpl α) α
pTakeAllBytes = pEither fromBytes pTakeAll

(<?>) ∷ ∀ π α . ByteArray π ⇒ Parser π α → String → Parser π α
(<?>) p s = Parser $ \(a ∷ π) → case (unBAP p) a of
    (Left e, _) → (Left ("in " ⊕ s ⊕ ": " ⊕  e), a)
    x → x

infixl 3 <?>

-- | This parser consumes no input. It never fails.
--
isEof ∷ ByteArray π ⇒ Parser π Bool
isEof = Parser $ \case
    a | length a ≡ 0 → (Right True, a)
      | otherwise → (Right False, a)

eof ∷ ByteArray π ⇒ Parser π ()
eof = Parser $ \case
    a| length a ≡ 0 → (Right (), a)
     | otherwise → (Left ("eof: remaining input: " ⊕ to16 a), a)

parse ∷ (Code16 π, ByteArray π) ⇒ Parser π α → π → Either String α
parse = parse' ""

parse' ∷ (Code16 π, ByteArray π) ⇒ String → Parser π α → π → Either String α
parse' s (Parser p) a = case p a of
    (Right r, a') → if length a' ≡ 0
        then Right r
        else Left $ "failed to consume all input while parsing" ⊕ ss ⊕ "; remaining bytes are: " ⊕ to16 a'
    (Left e, a') → Left $ "failed to parse" ⊕ ss ⊕ ": " ⊕ e ⊕ ". remaining bytes are: " ⊕ to16 a'
  where
    ss = if s ≡ "" then "" else " " ⊕ s

instance ByteArray π ⇒ Functor (Parser π) where
    fmap f (Parser p) = Parser $ first (fmap f) ∘ p

instance ByteArray π ⇒ Applicative (Parser π) where
    pure x = Parser $ \a → (Right x, a)
    (Parser p0) <*> (Parser p1) = Parser $ \a →
        case p0 a of
            (Left l, _) → (Left l, a)
            (Right r, a') → first (fmap r) $ (p1 a')

instance ByteArray π ⇒ Alternative (Parser π) where
    empty = Parser $ \s → (Left "empty", s)
    (<|>) a b = Parser $ \x → case unBAP a x of
        r@(Right {}, _) → r
        (Left s, _) → case unBAP b x of
            r'@(Right {}, _) → r'
            (Left s', t) → (Left ("[" ⊕ s ⊕ "," ⊕ s' ⊕ "]"), t)

instance ByteArray x => Monad (Parser x) where
    return  = pure
    a >>= b = Parser $ \x ->
        case unBAP a x of
            (Right r, bs') -> unBAP (b r) bs'
            (Left v, bs')  -> (Left v, bs')
