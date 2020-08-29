{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ((@?=), (@=?))
import Test.SmallCheck.Series

import Text.Emoji.Types
import Text.Emoji.DataFiles

import Data.Attoparsec.Text (parseOnly)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [typesTests, parserTests]

typesTests :: TestTree
typesTests = testGroup "Emoji Types Tests"
  [ SC.testProperty "EmojiVersion Ord instance is commutative" emojiVersionOrdCommutative
  , SC.testProperty "EmojiVersion Lowest Value" emojiVersionLowestValue
  ]

emojiVersionOrdCommutative :: EmojiVersion -> EmojiVersion -> Bool
emojiVersionOrdCommutative a b = (a >= b) == (b <= a)

emojiVersionLowestValue :: EmojiVersion -> Bool
emojiVersionLowestValue v = NoEmojiVersion Nothing <= v

instance Monad m => Serial m EmojiVersion

parserTests :: TestTree
parserTests = testGroup "Emoji Data File Parser Tests"
  [ HU.testCase "EmojiVersion parser parses all versions up till now correctly" emojiVersionParserTest
  , HU.testCase "emoji-test.txt parser collects all entries" emojiTestParserLinesTest
  ]

emojiVersionParserTest :: HU.Assertion
emojiVersionParserTest = do
  vt "E0.0" (NoEmojiVersion Nothing)
  vt "E0.6" (NoEmojiVersion (Just 6))
  vt "E0.7" (NoEmojiVersion (Just 7))
  vt "E1.0" (EmojiVersion 1 0)
  vt "E2.0" (EmojiVersion 2 0)
  vt "E3.0" (EmojiVersion 3 0)
  vt "E4.0" (EmojiVersion 4 0)
  vt "E5.0" (EmojiVersion 5 0)
  vt "E11.0" (EmojiVersion 11 0)
  vt "E12.0" (EmojiVersion 12 0)
  vt "E12.1" (EmojiVersion 12 1)
  vt "E13.0" (EmojiVersion 13 0)
  where vt :: Text -> EmojiVersion -> HU.Assertion
        vt str exp = parseOnly emojiVersionColumn str @?= Right exp

-- | Helper Function that counts number of lines used to parse 'EmojiTest'.
--   Useful to check against LoC of @emoji-test.txt@ for parser sanity check.
countLines :: EmojiTest -> Int
countLines ((Group _ _ x):xs) = 1 + countLines x + countLines xs
countLines ((Comment _):xs) = 1 + countLines xs
countLines ((Entry _ _ _ _):xs) = 1 + countLines xs
countLines [] = 0

emojiTestParserLinesTest :: HU.Assertion
emojiTestParserLinesTest =
  T.readFile "./data/emoji-test.txt" >>= \emojiTestTxt ->
    let lines = countLines <$> parseOnly emojiTestFile emojiTestTxt
        expectedLines = length . filter (/= "") $ T.lines emojiTestTxt
     in lines @?= Right expectedLines
