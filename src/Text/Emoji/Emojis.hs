{-# LANGUAGE TemplateHaskell #-}
module Text.Emoji.Emojis
  ( emojiData
  ) where

import Text.Emoji.Types
import Text.Emoji.Data
import Data.Either
import Data.FileEmbed
import Text.Parsec

emojiData :: [Emoji]
emojiData = toRights . (\(Right x) -> x) $ parse emojiDataFile "" $(embedStringFile "./emoji-data.txt")

toRights :: [Either a b] -> [b]
toRights = map (\(Right x) -> x) . filter isRight
