{-# LANGUAGE DeriveGeneric #-}
{-|
  Module:      Text.Emoji.Types
  Description: Basic types and abstractions of emoji
-}


module Text.Emoji.Types
  ( EmojiVersion (..)
  , EmojiStyle (..)
  , EmojiQualification (..)
  , EmojiStatus (..)
  , EmojiModifier (..)
  , EmojiSource (..)
  , Emoji (..)
  ) where

import Data.Word (Word32 ())
import GHC.Generics

data EmojiVersion
  = NoEmojiVersion (Maybe Integer) -- ^ Not applicable. Maybe may contain Unicode Version.
  | EmojiVersion Integer Integer   -- ^ @EmojiVersion Major Minor@
  deriving (Show, Eq, Generic)

instance Ord EmojiVersion where
  compare (EmojiVersion maj1 min1) (EmojiVersion maj2 min2) = compare (maj1, min1) (maj2, min2)
  compare (NoEmojiVersion Nothing) (NoEmojiVersion Nothing) = EQ
  compare (NoEmojiVersion Nothing) _ = LT
  compare _ (NoEmojiVersion Nothing) = GT
  compare (NoEmojiVersion (Just v1)) (NoEmojiVersion (Just v2)) = compare v1 v2
  compare (NoEmojiVersion _) (EmojiVersion _ _) = LT
  compare (EmojiVersion _ _) (NoEmojiVersion _) = GT

data EmojiQualification
  = FullyQualified
  | MinimallyQualified
  | Unqualified
  deriving (Show, Eq, Ord, Enum, Generic)

data EmojiStatus
  = EmojiStatusCharacter EmojiQualification
  | EmojiStatusComponent
  deriving (Show, Eq, Ord, Generic)

-- | Represents the default style
-- an emoji is displayed as.
-- See http://www.unicode.org/reports/tr51/index.html#Emoji_vs_Text_Display
data EmojiStyle
  = Emoji
  | Text
  deriving (Show, Eq, Ord, Enum, Generic)

-- | Describes if and how the emoji can be
-- used as modifier.
-- See: http://www.unicode.org/reports/tr51/index.html#Emoji_Modifiers
data EmojiModifier
  = Modifier
  | ModifierBase
  | ModifierSequence
  deriving (Show, Eq, Ord, Enum, Generic)

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
  deriving (Show, Eq, Ord, Enum, Generic)

data Emoji = MkEmoji
  { emojiCodePoints     :: [Word32]            -- ^ The code points of the unicode character.
  , emojiDefaultStyle   :: EmojiStyle          -- ^ The default display style.
  , emojiModifierStatus :: EmojiModifier       -- ^ Wether the emoji is a modifier.
  , emojiSources        :: [EmojiSource]       -- ^ Where the emoji originates.
  , emojiVersion        :: EmojiVersion        -- ^ Version the character was introduced.
  , emojiStaticName     :: String              -- ^ The Name of the character
  , emojiShortName      :: String              -- ^ CLDR short name of the character
  , emojiExplicitGender :: Bool                -- ^ Wether this Emoji has an explicit gender
  } deriving (Show, Eq, Ord, Generic)
