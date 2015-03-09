-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

{-# LANGUAGE UnicodeSyntax #-}

module PC.Bytes.Codec
( Code64(..)
, Code16(..)
) where

-- -------------------------------------------------------------------------- --
-- * Base64Url serialization

class Code64 α where
    to64 ∷ α → String
    from64 ∷ String → Either String α

-- -------------------------------------------------------------------------- --
-- * Hex serialization

class Code16 α where
    to16 ∷ α → String
    from16 ∷ String → Either String α

