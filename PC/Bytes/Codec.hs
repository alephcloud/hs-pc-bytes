-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

module PC.Bytes.Codec
    ( to16
    , from16
    , to64
    , from64
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Base64.URL    as B64

to16 :: ByteString -> ByteString
to16 = B16.encode

from16 :: ByteString -> Either String ByteString
from16 b =
    let (parsed, left) = B16.decode b
     in if B.null left then Right parsed else Left ("base16 decoding failed: " ++ show left)

to64 :: ByteString -> ByteString
to64 = B64.encode

from64 :: ByteString -> Either String ByteString
from64 = B64.decode
