{-|
    Module: Text.Emoji
    Description: Main module of the emoji library

Library for querying information about emojis, filtering and displaying emojis.
-}
module Text.Emoji
  ( emojiData
  , emojiToString
  , emojiToText
  , EmojiStyle (..)
  , EmojiLevel (..)
  , EmojiModifierStatus (..)
  , EmojiSource (..)
  , EmojiSources (..)
  , Emoji (..)
  ) where

import Text.Emoji.Emojis
import Text.Emoji.Text
import Text.Emoji.String
import Text.Emoji.Types
