module Aria where

import qualified Data.ByteString.Lazy as BS (fromStrict)
import qualified Data.ByteString.Char8 as BSC (pack, unpack)
import qualified Data.Text as T (unpack)

type AriaKey    = String
type AriaValue  = String

data AriaKV = AriaKV {key :: AriaKey, value :: AriaValue} deriving (Show, Read)

ariaKey AriaKV {key=k, value=_}   = k
ariaValue AriaKV {key=_, value=v} = v

textToAriaValue = BSC.unpack
textToAriaKey   = T.unpack
ariaToText      = BS.fromStrict . BSC.pack
