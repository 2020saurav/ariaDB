{-|
Module      : Aria
Description : Data types and related functions
License     : BSD3
Maintainer  : 2020saurav@gmail.com
Stability   : experimental

This module contains definition of type 'AriaKV' and important functions
required by 'WarpServer'.
-}
module Aria where

import qualified Data.ByteString.Lazy as BS (fromStrict, ByteString)
import qualified Data.ByteString.Char8 as BSC (pack, unpack, ByteString)
import qualified Data.Text as T (unpack, Text)

-- | Type to capture key for AriaDB
type AriaKey    = String

-- | Type to capture value for AriaDB
type AriaValue  = String

-- | Type to capture key-value pair for AriaDB
data AriaKV = AriaKV {key :: AriaKey, value :: AriaValue} deriving (Show, Read)

-- | It returns the 'key' in key-value pair
ariaKey :: AriaKV -> AriaKey
ariaKey AriaKV {key=k, value=_}   = k

-- | It returns the 'value' in key-value pair
ariaValue :: AriaKV -> AriaValue
ariaValue AriaKV {key=_, value=v} = v

-- | It converts ByteString text to AriaValue (String)
textToAriaValue :: BSC.ByteString -> AriaValue
textToAriaValue = BSC.unpack

-- | It converts Text to AriaKey (String)
textToAriaKey :: T.Text -> AriaKey
textToAriaKey = T.unpack

-- | It converts AriaValue/AriaKey to ByteString
ariaToText :: String -> BS.ByteString
ariaToText = BS.fromStrict . BSC.pack
