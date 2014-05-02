module Misc.Output where

import Data.ByteString.Lazy(ByteString)

class Output a where
  toByteString :: a -> ByteString

