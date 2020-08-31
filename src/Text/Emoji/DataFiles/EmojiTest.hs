{-# LANGUAGE OverloadedStrings #-}
{-|
  Module:      Text.Emoji.DataFiles.EmojiTest
  Description: Parsers and Utilities related to the @emoji-test.txt@ file.

  This module can be utilized to parse the @emoji-test.txt@ data file
  provided by the Unicode Consortium. This file contains a representation
  of a more or less typical emoji keyboard which means you get:

  * a list of valid emojis (qualified) plus unqualified ones,
    emoji components, you can expect to find in the wild or
    may want to input
  * an organization of those into groups and subgroups (like in a keyboard)
-}


module Text.Emoji.DataFiles.EmojiTest
  ( -- * Representation of the parsed file
    EmojiTest ()
  , EmojiTestEntry (..)
  , EmojiTestGroupLevel (..)
    -- * Attoparsec parsers
  , emojiVersionColumn
  , emojiTestFile
  , manyEmojiTestEntries
  ) where

import Prelude hiding (takeWhile)

import Text.Emoji.Types

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser (), takeWhile1, takeWhile, string, choice
                            , notInClass, skipWhile, skipMany, isHorizontalSpace
                            , decimal, hexadecimal, char, many1, endOfLine, endOfInput)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

type EmojiTest = [EmojiTestEntry]

data EmojiTestEntry
  = Group EmojiTestGroupLevel Text [EmojiTestEntry]
  | Entry [Word32] EmojiStatus EmojiVersion Text
  | Comment Text
  deriving (Show, Eq, Ord)

data EmojiTestGroupLevel
  = EmojiTestGroup
  | EmojiTestSubgroup
  deriving (Show, Eq, Ord, Enum)

groupLevelText :: EmojiTestGroupLevel -> Text
groupLevelText EmojiTestGroup = "group"
groupLevelText EmojiTestSubgroup = "subgroup"

notEol :: Char -> Bool
notEol = notInClass "\n"

skipSpace :: Parser ()
skipSpace = skipWhile isHorizontalSpace

codePointsColumn :: Parser [Word32]
codePointsColumn = many1 (hexadecimal <* char ' ')

statusColumn :: Parser EmojiStatus
statusColumn =
  (string "fully-qualified" >> pure (EmojiStatusCharacter FullyQualified)) <|>
  (string "minimally-qualified" >> pure (EmojiStatusCharacter MinimallyQualified)) <|>
  (string "unqualified" >> pure (EmojiStatusCharacter Unqualified)) <|>
  (string "component" >> pure (EmojiStatusComponent))

emojiTestGroup :: EmojiTestGroupLevel -> Parser EmojiTestEntry
emojiTestGroup maxLevel = do
  _ <- char '#'
  skipSpace

  _ <- string $ groupLevelText maxLevel
  _ <- char ':'
  skipSpace

  name <- takeWhile1 notEol
  skipMany endOfLine

  let groupParser =
        if maxLevel == EmojiTestGroup
          then [ emojiTestGroup EmojiTestSubgroup ]
          else []

  groupEntries <- many1 . choice $
    groupParser ++ [ emojiTestEntryLine, emojiTestCommentLine ]

  pure $ Group EmojiTestGroup name groupEntries

-- | Parses the textual representation of an 'EmojiVersion' as found
--   in @emoji-test.txt@.
emojiVersionColumn :: Parser EmojiVersion
emojiVersionColumn = do
  _ <- char 'E'
  major <- decimal
  _ <- char '.'
  minor <- decimal
  pure $ case major of
           0 -> case minor of
                  -- E0.0: pre emoji without specific Unicode Version
                  0 -> NoEmojiVersion Nothing
                  -- E0.x: Pre emoji with Unicode Version
                  _ -> NoEmojiVersion (Just minor)
           -- Ex.y: Regular Emoji Version
           _ -> EmojiVersion major minor

emojiTestEntryLine :: Parser EmojiTestEntry
emojiTestEntryLine = do
  codePoints <- codePointsColumn
  skipSpace

  _ <- string "; "
  status <- statusColumn
  skipSpace

  _ <- string "# "
  skipWhile (notInClass "E")
  version <- emojiVersionColumn
  skipSpace

  shortName <- takeWhile1 notEol
  skipMany endOfLine

  pure $ Entry codePoints status version shortName

emojiTestCommentLine :: Parser EmojiTestEntry
emojiTestCommentLine = do
  _ <- char '#'
  skipSpace
  text <- takeWhile notEol <* skipMany endOfLine
  if "group:" `T.isPrefixOf` text || "subgroup:" `T.isPrefixOf` text
    then fail "group, not comment"
    else pure $ Comment text

-- | Parses an entire @emoji-test.txt@ file and consumes the end of input.
emojiTestFile :: Parser EmojiTest
emojiTestFile = manyEmojiTestEntries <* endOfInput

-- | Parses one or more 'EmojiTestEntries'.
manyEmojiTestEntries :: Parser EmojiTest
manyEmojiTestEntries = many1 $
  emojiTestGroup EmojiTestGroup <|> emojiTestEntryLine <|> emojiTestCommentLine
