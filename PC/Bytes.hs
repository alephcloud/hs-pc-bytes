-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

module PC.Bytes
    (
    -- * Types
      Bytes(..)
    , BackendByteArray
    , ByteString
    -- * Hexadecimal and Base64 conversion
    , toBytesBase64
    , toBytesBase16
    , fromBytesBase64
    , fromBytesBase16
    -- * Random Bytes generation
    , getRandom
    ) where

import PC.Bytes.ByteArray
import PC.Bytes.Random
import Data.ByteString (ByteString)
