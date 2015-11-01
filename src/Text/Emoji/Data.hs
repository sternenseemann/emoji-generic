{-|
  Module:      Text.Emoji.Data
  Description: Parser for the emoji-data file format

This module defines the Parsec parser necessary
to parse the emoji-data.txt file issued by the
unicode consortium.
-}


{-# LANGUAGE OverloadedStrings #-}
module Text.Emoji.Data where

import           Text.Emoji.Types

import           Control.Applicative ((<$), (<$>))
import           Data.Char           (isSpace)
import           Data.List           (dropWhile, dropWhileEnd)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromJust, isJust, catMaybes)
import           Text.Parsec
import           Text.Parsec.String
import           Numeric             (readHex)

-- | Parses the entire emoji-data.txt file.
-- Left String is a comment line.
-- Right Emoji is a line describing an emoji character.
emojiDataFile :: Parser [Either String Emoji]
emojiDataFile = many $ emojiCommentLine <|> emojiDataEntry

-- | Parsers an comment line.
emojiCommentLine :: Parser (Either String Emoji)
emojiCommentLine = do
  string "#"
  r <- manyTill anyChar (try lineTerminated)
  return $ Left r

-- | Parsers an emoji data entry and returns a
-- Left Emoji using all the information given.
emojiDataEntry :: Parser (Either String Emoji)
emojiDataEntry = do
  code <- map (fst . head . readHex) . splitOn " " <$> field hexDigit
  style <- sumTypeField emojiStyles
  level <- sumTypeField emojiLevels
  modifier <- sumTypeField emojiModifierStati
  sourceList <- sources

  string "\t# "

  version <- many (noneOf " ")
  space >> char '(' >> manyTill anyChar (char ')') >> space
  name <- manyTill anyChar (try lineTerminated)

  return . Right $ MkEmoji
    { _code           = code
    , _defaultStyle   = style
    , _emojiLevel     = level
    , _emojiModifier  = modifier
    , _emojiSources   = sourceList
    , _name           = version
    , _version        = name
    }

-- | Parses the emoji sources field.
-- TODO: Handle NA correctly (no problem right now though).
sources :: Parser EmojiSources
sources = catMaybes .
  map (flip lookup (emojiSources)) .
  splitOn " " . dropAround isSpace <$>
  many (oneOf (map (head . fst) emojiSources) <|> char ' ')

-- | Matches the separator sequence.
separator :: Parser String
separator = string ";\t" <|> string " ;\t"

-- | Uses a parser association list to convert
-- to a sum type.
sumType :: ParserAssoc a -> Parser a
sumType assocs = choice [r <$ (try . string $ s) | (s,r) <- assocs]

-- | Identical to sumType except that it matches
-- a separator afterwards.
sumTypeField :: ParserAssoc a -> Parser a
sumTypeField assocs = do
  res <- sumType assocs
  separator
  return res

-- | Matches a emoji-data field consisting of spaces and
-- Chars matched by the given Parser. Whitespace gets
-- stripped. Returns the matched String.
field :: Parser Char -> Parser String
field p = do
  res <- many (p <|> char ' ')
  separator
  return $ dropAround isSpace res

-- | Drops elements that match a given condition
-- at the end and beginning of a list.
dropAround :: (a -> Bool) -> [a] -> [a]
dropAround f = dropWhileEnd f . dropWhile f

-- | Matches an end of line or an end of file.
lineTerminated :: Parser ()
lineTerminated = eof <|> () <$ endOfLine
