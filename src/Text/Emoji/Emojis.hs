{-# LANGUAGE TemplateHaskell #-}
{-|
    Module:      Text.Emoji.Emojis
    Description: Exports the representation of all Emojis as list

Exports the parsed emojiData.
-}

module Text.Emoji.Emojis
  ( emojiData
  ) where

import Text.Emoji.Types
import Text.Emoji.Data
import Data.Either
import Data.FileEmbed
import Text.Parsec

-- | All Emojis listed in emoji-data.txt in a list.
--   Warning: The data is parsed at run time although emoji-data.txt
--   is not needed at run time. Hopefully parsing will be done at compile
--   time in the future.
emojiData :: [Emoji]
emojiData = toRights . (\(Right x) -> x) $ parse emojiDataFile "" $(embedStringFile "./emoji-data.txt")

toRights :: [Either a b] -> [b]
toRights = map (\(Right x) -> x) . filter isRight
