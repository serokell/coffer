-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.EntryView
  ( FieldInfoView (..)
  , fieldInfo
  , private
  , EntryView (..)
  , mQEntryPath
  , entryTags
  , fields
  , parseEntryView
  ) where

import CLI.Parser (MParser, endOfLineOrFile)
import CLI.Types (FieldInfo(FieldInfo, fiContents, fiName))
import Coffer.Path (EntryPath, QualifiedPath, mkQualifiedEntryPath)
import Control.Applicative (Alternative(many, (<|>)), empty)
import Control.Lens
import Control.Monad (void)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Entry (EntryTag, FieldValue(FieldValue), fieldValue, newEntryTag, newFieldKey)
import Fmt (Buildable(build), unlinesF)
import Text.Interpolation.Nyan
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as Lexer

data FieldInfoView = FieldInfoView
  { fivFieldInfo :: FieldInfo
  , fivPrivate   :: Bool
  }
  deriving stock (Show, Eq)
makeLensesWith abbreviatedFields ''FieldInfoView

instance Buildable FieldInfoView where
  build (FieldInfoView fieldInfo private) = build fieldName <> delimeter <> buildedFieldContents
    where
      fieldName = fiName fieldInfo
      fieldContents = fiContents fieldInfo

      buildedFieldContents
        | _ : _ : _ <- T.split (== '\n') (fieldContents ^. fieldValue) = [int|s|
"""
#{fieldContents}
"""
|]
        | otherwise = build fieldContents

      delimeter
        | private = " =~ "
        | otherwise = " = "

data EntryView = EntryView
  { evMQEntryPath :: Maybe (QualifiedPath EntryPath)
  , evFields     :: [FieldInfoView]
  , evEntryTags  :: Set EntryTag
  }
  deriving stock (Show, Eq)
makeLensesWith abbreviatedFields ''EntryView

instance Buildable EntryView where
  build (EntryView mQEntryPath fields entryTags) = [int|s|
#{buildedPath}

[fields]
#{buildedFields}
[tags]
#{buildedEntryTags}
|]
    where
      buildedPath =
        case mQEntryPath of
          Nothing -> "path = # <-- write your qualified entry path here"
          Just qPath -> "path = " <> build qPath

      buildedFields =
        fields
          <&> build
          & unlinesF

      buildedEntryTags =
        entryTags
          & S.toList
          <&> build
          & unlinesF

spaceConsumer :: MParser ()
spaceConsumer = Lexer.space
  P.space1
  (Lexer.skipLineComment "#")
  empty

lexeme :: MParser a -> MParser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> MParser Text
symbol = Lexer.symbol spaceConsumer

eitherToMParser :: Int -> Either Text a -> MParser a
eitherToMParser offset = either failAction pure
  where
    failAction :: Text -> MParser a
    failAction errMsg = do
      P.setOffset offset
      fail $ T.unpack errMsg

parseQualifiedPath :: MParser (Maybe (QualifiedPath EntryPath))
parseQualifiedPath = do
  offset <- P.getOffset
  void $ symbol "path" >> symbol "="
  qPathStr <- many (P.notFollowedBy P.space1 >> P.anySingle) <&> T.strip . T.pack
  eitherToMParser offset (mkQualifiedEntryPath qPathStr) <&> Just

parseFieldInfoView :: MParser FieldInfoView
parseFieldInfoView = do
  offset <- P.getOffset
  fieldNameStr <- lexeme $ many (P.notFollowedBy (P.space1 <|> void (P.char '=')) >> P.anySingle) <&> T.pack
  fieldName <- eitherToMParser offset $ newFieldKey fieldNameStr
  delimeter <- P.string "=~" <|> P.string "="
  void $ many P.hspace1
  fieldContents <- lexeme (parseFieldContentsTripleQuotes <|> parseFieldContentsSingleLine)

  let fieldInfo = FieldInfo fieldName fieldContents
  let private = delimeter == "=~"

  pure $ FieldInfoView fieldInfo private
  where
    -- | Parse the rest of the line as a field content.
    parseFieldContentsSingleLine :: MParser FieldValue
    parseFieldContentsSingleLine = FieldValue . T.pack <$> P.manyTill P.anySingle endOfLineOrFile

    -- | Parse a field content wrapped in triple quotes @"""@. E.g.:
    --
    -- > """
    -- > line1
    -- > line2
    -- > """
    parseFieldContentsTripleQuotes :: MParser FieldValue
    parseFieldContentsTripleQuotes = do
      let beginBlock = tripleQuote >> void P.eol
      let parseLine = P.manyTill P.anySingle P.eol
      let endBlock = tripleQuote >> endOfLineOrFile

      res <- beginBlock >> P.manyTill parseLine endBlock
      pure $ res <&> T.pack & T.intercalate "\n" & FieldValue

      where
        tripleQuote :: MParser ()
        tripleQuote = void $ P.string "\"\"\""

parseEntryTag :: MParser EntryTag
parseEntryTag = do
  offset <- P.getOffset
  entryTagStr <- lexeme $ P.manyTill P.anySingle endOfLineOrFile <&> T.pack
  eitherToMParser offset $ newEntryTag entryTagStr

parseEntryView :: MParser EntryView
parseEntryView = do
  spaceConsumer
  qEntryPath <- lexeme parseQualifiedPath
  void $ symbol "[fields]"
  fieldInfoViews <- P.manyTill (lexeme parseFieldInfoView) (P.char '[')
  void $ symbol "tags]"
  entryTags <- lexeme (many parseEntryTag) <&> S.fromList
  endOfLineOrFile
  pure $ EntryView qEntryPath fieldInfoViews entryTags
