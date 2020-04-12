{-|
  Module:      Text.Emoji.Text
  Description: Emoji utilities for Text
-}


module Text.Emoji.Text where

import           Text.Emoji.String
import           Text.Emoji.Types

import           Data.Text         (Text (), pack)

-- Uses the implementation of Text.Emoji.String
-- since Text uses Chars, too.

-- | Ouputs the given Emoji into Text.
emojiToText :: Emoji -> Text
emojiToText = pack . emojiToString
