{-# LANGUAGE TemplateHaskell #-}
module Text.Emoji.Types where

import           Data.Char (toLower)
import           GHC.Word  (Word64 ())

data EmojiStyle
  = Emoji
  | Text
  deriving Show

data EmojiLevel
  = L1
  | L2
  | NA
  deriving Show

data EmojiModifierStatus
  = Modifier
  | Primary
  | Secondary
  | None
  deriving (Show)

data EmojiSource
  = ZDings
  | ARIB
  | JCarrier
  | WDings
  | X -- FIXME: Long Name?
  deriving (Show)

type ParserAssoc a = [(String, a)]

tupleShow :: Show a => (String -> String) -> a -> (String, a)
tupleShow f x = (f . show $ x, x)

emojiStyles :: ParserAssoc EmojiStyle
emojiStyles = map (tupleShow $ map toLower) [Emoji, Text]

emojiLevels :: ParserAssoc EmojiLevel
emojiLevels = map (tupleShow id) [L1, L2, NA]

emojiModifierStati :: ParserAssoc EmojiModifierStatus
emojiModifierStati = map (tupleShow $ map toLower) [Modifier, Primary, Secondary, None]

emojiSources :: ParserAssoc EmojiSource
emojiSources = [ ("z", ZDings), ("a", ARIB), ("j", JCarrier), ("w", WDings)
               , ("x", X) ]

type EmojiSources = [EmojiSource]

data Emoji = MkEmoji
  { _code          :: Word64
  , _defaultStyle  :: EmojiStyle
  , _emojiLevel    :: EmojiLevel
  , _emojiModifier :: EmojiModifierStatus
  , _emojiSources  :: EmojiSources
  , _version       :: String
  , _name          :: String
  } deriving Show
