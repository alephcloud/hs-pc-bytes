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
