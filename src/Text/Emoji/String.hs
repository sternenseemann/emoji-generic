{-|
  Module:      Text.Emoji.String
  Description: Emoji utilities for String
-}

module Text.Emoji.String where

import           Text.Emoji.Types

import           Codec.Binary.UTF8.Light (w2c)

-- | Outputs a String for the given Emoji.
emojiToString :: Emoji -> String
emojiToString = map w2c . emojiCodePoints
