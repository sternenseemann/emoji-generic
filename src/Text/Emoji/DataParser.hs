{-# LANGUAGE OverloadedStrings #-}
module Text.Emoji.DataParser where

import           Text.Emoji.Types

import           Control.Applicative ((<$), (<$>))
import           Data.Char           (isSpace)
import           Data.List           (dropWhile, dropWhileEnd)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromJust, isJust, catMaybes)
import           Text.Parsec
import           Text.Parsec.String
import           Numeric             (readHex)

emojiDataFile :: Parser [Either String Emoji]
emojiDataFile = many $ emojiCommentLine <|> emojiDataEntry

emojiCommentLine :: Parser (Either String Emoji)
emojiCommentLine = Left <$> emojiComment

emojiComment :: Parser String
emojiComment = do
  string "#"
  r <- manyTill anyChar (try lineTerminated)
  return r

emojiDataEntry :: Parser (Either String Emoji)
emojiDataEntry = do
  code <- fst . head . readHex . filter (not . isSpace) <$> field hexDigit
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

sources :: Parser EmojiSources
sources = catMaybes .
  map (flip lookup (emojiSources)) .
  splitOn " " . dropAround isSpace <$>
  many (oneOf (map (head . fst) emojiSources) <|> char ' ')

separator :: Parser String
separator = string ";\t" <|> string " ;\t"

sumType :: ParserAssoc a -> Parser a
sumType assocs = choice [r <$ (try . string $ s) | (s,r) <- assocs]

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
