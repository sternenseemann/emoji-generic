{-|
    Module: Text.Emoji
    Description: Main module of the emoji library

Library for querying information about emojis, filtering and displaying emojis.
-}
module Text.Emoji
  ( emojiStandardVersion
  ) where

import Text.Emoji.Types

emojiStandardVersion :: EmojiVersion
emojiStandardVersion = EmojiVersion 13 0

