-- Copyright (c) 2013-2014 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.
--

module PC.Bytes.Random
    ( getRandom
    ) where

import Control.Applicative
import Crypto.Random
import Data.ByteString (ByteString)

getRandom :: Int -> IO ByteString
getRandom n = do
    rng <- cprgCreate <$> createEntropyPool :: IO SystemRNG
    return $ fst . cprgGenerate n $ rng