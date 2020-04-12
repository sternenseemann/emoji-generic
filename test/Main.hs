{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series

import Text.Emoji.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [typesTests]

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
