{-|
  Module:      Text.Emoji.Text
  Description: Emoji utilities for Text
-}


module Text.Emoji.Text where

import qualified Text.Emoji.String as S
import           Text.Emoji.Types

import           Data.Text         (Text (), pack)

-- | Ouputs the given Emoji into Text.
-- Uses the implementation of Text.Emoji.String
-- since Text uses Chars, too.
fromEmoji :: Emoji -> Text
fromEmoji = pack . S.fromEmoji
