-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- PC.Bytes.Codec
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
--
-- |
-- Module      : PC.Bytes.Codec
-- Copyright   : (c) 2013-2015 PivotCloud, Inc
-- License     : Apache-2, see LICENSE file of the package
-- Maintainer  : licensing@pivotmail.com
--

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
