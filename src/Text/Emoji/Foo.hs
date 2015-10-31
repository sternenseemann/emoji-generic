{-# LANGUAGE OverloadedStrings #-}
module Text.Emoji.DataParser where

import           Prelude              hiding (takeWhile)

import           Text.Emoji.Types

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Text            (Text ())
import qualified Data.Text            as T

emojiData :: Parser [[Text]]
emojiData = many' (emojiDataLine <|> commentLine)

commentLine :: Parser [Text]
commentLine = do
  "# "
  comment <- takeWhile (not . isEndOfLine)
  endOfLine
  return $ [comment]

emojiDataLine :: Parser [Text]
emojiDataLine = do
  hex <- field
  style <- field
  level <- field
  modifier <- field
  sources <- field
  comment <- field
  endOfLine <|> endOfInput
  return $ [hex, style, level, modifier, sources, comment]

field :: Parser Text
field = do
  res <- T.dropAround (\c -> c == '\t' || c == ' ') <$> takeWhile (/= ';')
  char ';' <|> endOfInput
  return res
