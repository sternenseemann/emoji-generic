{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.HUnit as HU
import Test.SmallCheck.Series

import Text.Emoji.Types
import Text.Emoji.DataFiles

import Data.Attoparsec.Text (parseOnly)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T (unpack)

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
  [ HU.testCase "EmojiVersion Parser parses all versions up till now correctly" emojiVersionParserTest ]

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
        vt str exp = HU.assertBool (T.unpack str) . fromRight False $
          (== exp) <$> parseOnly emojiVersionColumn str
