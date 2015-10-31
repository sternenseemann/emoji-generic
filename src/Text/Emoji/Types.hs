{-|
  Module:      Text.Emoji.Types
  Description: Basic types and abstractions of emoji
-}


{-# LANGUAGE TemplateHaskell #-}
module Text.Emoji.Types where

import           Data.Char (toLower)
import           GHC.Word  (Word64 ())

-- | Represents the default style
-- an emoji is displayed as.
-- See http://www.unicode.org/reports/tr51/index.html#Emoji_vs_Text_Display
data EmojiStyle
  = Emoji
  | Text
  deriving Show

-- | Describes how common an emoji is.
-- See http://www.unicode.org/reports/tr51/index.html#Emoji_Levels
data EmojiLevel
  = L1
  | L2
  | NA
  deriving Show

-- | Describes if and how the emoji can be
-- used as modifier.
-- See: http://www.unicode.org/reports/tr51/index.html#Emoji_Modifiers
data EmojiModifierStatus
  = Modifier
  | Primary
  | Secondary
  | None
  deriving (Show)

-- | Represents the source(s) the emoji came from.
-- The Unicode standard created an universal encoding
-- for different already existing symbols and characters.
-- For example Wingdings symbols were added to the Unicode emojis.
-- See http://www.unicode.org/reports/tr51/#Major_Sources
data EmojiSource
  = ZDings
  | ARIB
  | JCarrier
  | WDings
  | X -- FIXME: Long Name?
  deriving (Show)

-- | An associative list to hold relations
-- between a string and an element of type a
-- in order to convert strings into as.
type ParserAssoc a = [(String, a)]

-- | Automatically generate an association tuple by
-- using show. Addtionally the result of show is applied to
-- f to do corrections of the derived show output.
tupleShow :: Show a => (String -> String) -> a -> (String, a)
tupleShow f x = (f . show $ x, x)

-- | Parser association list for EmojiStyle
emojiStyles :: ParserAssoc EmojiStyle
emojiStyles = map (tupleShow $ map toLower) [Emoji, Text]

-- | Parser association list for EmojiLevel
emojiLevels :: ParserAssoc EmojiLevel
emojiLevels = map (tupleShow id) [L1, L2, NA]

-- | Parser association list for EmojiModifierStatus
emojiModifierStati :: ParserAssoc EmojiModifierStatus
emojiModifierStati = map (tupleShow $ map toLower)
  [Modifier, Primary, Secondary, None]

-- | Parser association list for EmojiSource
emojiSources :: ParserAssoc EmojiSource
emojiSources = [ ("z", ZDings), ("a", ARIB), ("j", JCarrier), ("w", WDings)
               , ("x", X) ]

-- | Since a Emoji might come from multiple sources
-- these are represented as a list of EmojiSource.
-- Empty List means 'NA' (not applicable).
type EmojiSources = [EmojiSource]

-- | Emoji holds all the information about an
-- emoji provided by emoji-data.txt
data Emoji = MkEmoji
  { _code          :: Word64              -- ^ The code of the unicode character.
  , _defaultStyle  :: EmojiStyle          -- ^ The default display style.
  , _emojiLevel    :: EmojiLevel          -- ^ Commonness for the character.
  , _emojiModifier :: EmojiModifierStatus -- ^ Wether the emoji is a modifier.
  , _emojiSources  :: EmojiSources        -- ^ Where the emoji originates.
  , _version       :: String              -- ^ Version the character was introduced.
  , _name          :: String              -- ^ The Name of the character
  } deriving Show
