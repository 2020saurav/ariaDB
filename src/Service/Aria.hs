module Aria where

import qualified Data.ByteString.Lazy as BS (fromStrict)
import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.Text as T (unpack)

type AriaKey    = String
type AriaValue  = String

data AriaKV = AriaKV {key :: AriaKey, value :: AriaValue} deriving Show

ariaKey AriaKV {key=k, value=_} = k
ariaValue AriaKV {key=_, value=v} = v

textToAriaValue = BS.fromStrict
textToAriaKey   = T.unpack
ariaToText      = BS.fromStrict . BSC.pack
