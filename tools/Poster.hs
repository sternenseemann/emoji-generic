{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Emoji.Types
import Text.Emoji.DataFiles.EmojiTest

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad (join)
import Data.Attoparsec.Text (parse, feed, IResult (..))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Ratio
import Data.Word
import Numeric (showHex, fromRat, showFFloat)

import Conduit

import qualified Data.XML.Types as XT
import qualified Text.XML.Stream.Parse as XML
import qualified Text.XML.Stream.Render as XML

import qualified Options.Applicative as O

import System.Environment
import System.Exit
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO

flattenFilter :: EmojiTestEntry -> [EmojiTestEntry]
flattenFilter x@(Entry _ _ _ _) = [x]
flattenFilter   (Group _ _ gs)  = concatMap flattenFilter gs
flattenFilter   (Comment _)     = []

data PosterException
  = ParseError String
  deriving Show

instance Exception PosterException where
  displayException (ParseError e) = "Parse Error: " ++ e

parseEmojiTest :: MonadThrow m => ConduitT T.Text EmojiTestEntry m ()
parseEmojiTest = do
  parseResult <- (flip feed) T.empty <$> -- send end of input
    foldlC updateParser (Partial (parse emojiTestFile))
  case parseResult of
    Partial _ -> throwM $ ParseError "Not enough input"
    Fail _ _ m -> throwM $ ParseError m
    Done _ r -> yieldMany r
  where updateParser r t =
          case r of
            Partial _ -> feed r t
            _         -> r

svgPath :: Config -> EmojiTestEntry -> FilePath
svgPath cfg (Entry codes _ _ _) = cfgSvgPath cfg </> filename
  where filename = prefix ++ codes_string ++ ".svg"
        (prefix, con) = case cfgFontType cfg of
                          Twemoji -> ("", '-')
                          Noto    -> ("emoji_u", '_')
        codes_string = tail . foldr (\f acc -> con:(f acc)) "" $
          map showHex codes
svgPath _ _ = error "svgPath should receive only entries"

badElement :: XT.Name -> Bool
badElement n = XT.nameLocalName n == "image"

filterXMLEvent :: XT.Event -> Bool
filterXMLEvent ev =
  case ev of
    XT.EventBeginDocument -> False
    XT.EventEndDocument -> False
    XT.EventBeginDoctype _ _ -> False
    XT.EventEndDoctype -> False
    XT.EventInstruction _ -> False
    XT.EventComment _ -> False
    -- no image inclusions (only 4 times in noto or something)
    XT.EventBeginElement n _ -> not (badElement n)
    XT.EventEndElement n -> not (badElement n)
    _ -> True

data SVGState
  = SVGState
  { svgEmojiWidth :: Rational     -- ^ width in cm
  , svgEmojiHeight :: Rational    -- ^ height in cm
  , svgXEmojiCount :: Integer
  , svgXIndex :: Integer
  , svgYIndex :: Integer
  }

advance :: SVGState -> SVGState
advance st =
  if svgXEmojiCount st == svgXIndex st + 1
    then st { svgXIndex = 0
            , svgYIndex = svgYIndex st + 1 }
    else st { svgXIndex = svgXIndex st + 1 }


type XMLAttrs = [(XT.Name, [XT.Content])]
setAttribute :: XT.Name -> XT.Content -> XMLAttrs -> XMLAttrs
setAttribute n c [] = [(n, [c])]
setAttribute n c ((name,content):xs) =
  if name == n
    then (n, [c]) : xs
    else (name, content) : setAttribute n c xs

ratC :: Rational -> XT.Content
ratC r = XT.ContentText .
  (<> "cm") . T.pack $ (showFFloat (Just 2) . fromRat) r ""

svgPosition :: SVGState -> XT.Event -> XT.Event
svgPosition st ev =
  case ev of
    XT.EventBeginElement n attrs ->
      let w = svgEmojiWidth st
          h = svgEmojiHeight st
          x = w * fromIntegral (svgXIndex st)
          y = h * fromIntegral (svgYIndex st)
       in if XT.nameLocalName n == "svg"
            then XT.EventBeginElement n
               $ setAttribute "width"  (ratC w)
               . setAttribute "height" (ratC h)
               . setAttribute "x"      (ratC x)
               . setAttribute "y"      (ratC y)
               $ attrs
            else ev
    _ -> ev

concatXMLEvs :: Monad m => Config -> ConduitT [XT.Event] XT.Event m ()
concatXMLEvs cfg =
  let a0Width = 841 % 10
      a0Height = 1189 % 10
      emojiPerRow = cfgEmojiPerRow cfg
      emojiSide = a0Width / fromIntegral emojiPerRow
      initialState = SVGState
        { svgEmojiWidth = emojiSide
        , svgEmojiHeight = emojiSide
        , svgXEmojiCount = emojiPerRow
        , svgXIndex = 0
        , svgYIndex = 0
        }
      addSVG :: [XT.Event] -> SVGState -> (SVGState, [XT.Event])
      addSVG els st = (advance st, map (svgPosition st) els)
   in do
     yield XT.EventBeginDocument
     yield $ XT.EventBeginElement "{http://www.w3.org/2000/svg}svg"
       [ ("width", [ ratC a0Width ])
       , ("height", [ ratC a0Height ]) ]
     concatMapAccumC addSVG initialState
     yield $ XT.EventEndElement "{http://www.w3.org/2000/svg}svg"
     yield XT.EventEndDocument

buildSVG :: Config -> IO ()
buildSVG cfg = withSourceFile (cfgEmojiTest cfg) $ \source -> do
  runConduit
    $ source
   .| decodeUtf8C
   .| parseEmojiTest
   .| concatMapC flattenFilter
   .| mapC (svgPath cfg)
   .| filterMC doesFileExist
   .| mapMC (\f -> withSourceFile f $ \xml -> runConduit $ xml
        .| XML.detectUtf
        .| XML.parseText XML.def
        .| filterC filterXMLEvent
        .| sinkList)
   .| concatXMLEvs cfg
   .| XML.renderBytes XML.def
   .| stdoutC

data FontType = Twemoji | Noto

data Config
  = Config
  { cfgSvgPath :: FilePath
  , cfgEmojiTest :: FilePath
  , cfgEmojiPerRow :: Integer
  , cfgFontType :: FontType
  }

config :: O.Parser Config
config = Config
  <$> O.strOption
     (O.long "svg-path"
   <> O.metavar "PATH"
   <> O.help "Directory containing the font's svgs")
  <*> O.strOption
     (O.long "emoji-test"
   <> O.metavar "PATH"
   <> O.help "Path to emoji-test.txt")
  <*> O.option O.auto
     (O.long "per-row"
   <> O.metavar "INT"
   <> O.value 100
   <> O.help "how many emojis per row")
  <*> (O.flag' Twemoji (O.long "twemoji" <> O.help "SVGs are from twemoji") <|>
       O.flag' Noto    (O.long "noto"    <> O.help "SVGs are from noto-emoji"))

main :: IO ()
main = O.execParser opts >>= buildSVG
  where opts = O.info config O.fullDesc
