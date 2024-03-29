-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.Types where

import Coffer.Directory (Directory)
import Coffer.Path (EntryPath, Path, QualifiedPath)
import Coffer.Util (MParser)
import Control.Lens hiding (noneOf)
import Control.Monad (guard, void)
import Data.Bifunctor (first)
import Data.Char qualified as Char
import Data.Fixed (Pico)
import Data.Functor (($>))
import Data.OpenApi
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar.Month.Compat (Month, fromYearMonthValid)
import Data.Time.Compat
  (Day, LocalTime(LocalTime), UTCTime, Year, fromGregorianValid, localTimeToUTC, makeTimeOfDayValid,
  utc)
import Entry (Entry, EntryTag, Field, FieldContents(..), FieldName, FieldVisibility, newFieldName)
import Fmt (pretty)
import GHC.Generics (Generic)
import Options.Applicative
import Servant (FromHttpApiData(parseUrlPiece))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Options = Options
  { oConfigPathMb :: Maybe FilePath
  , oSomeCommand :: SomeCommand
  }
  deriving stock Show

data Command res where
  CmdView :: ViewOptions -> Command ViewResult
  CmdCreate :: CreateOptions -> Command CreateResult
  CmdSetField :: SetFieldOptions -> Command SetFieldResult
  CmdDeleteField :: DeleteFieldOptions -> Command DeleteFieldResult
  CmdFind :: FindOptions -> Command (Maybe Directory)
  CmdRename :: RenameOptions -> Command RenameResult
  CmdCopy :: CopyOptions -> Command CopyResult
  CmdDelete :: DeleteOptions -> Command DeleteResult
  CmdTag :: TagOptions -> Command TagResult

deriving stock instance Show (Command res)

data SomeCommand where
  SomeCommand :: Command res -> SomeCommand

deriving stock instance Show SomeCommand

----------------------------------------------------------------------------
-- Command results
----------------------------------------------------------------------------

data ViewResult
  = VRDirectory Directory
  | VREntry Entry
  | VRField FieldName Field
  | VRPathNotFound (QualifiedPath Path)
  | VRDirectoryNoFieldMatch (QualifiedPath Path) FieldName
  | VREntryNoFieldMatch (QualifiedPath EntryPath) FieldName
  deriving stock (Show)

data CreateError
  = CEParentDirectoryIsEntry (QualifiedPath EntryPath, QualifiedPath EntryPath)
  | CEDestinationIsDirectory (QualifiedPath EntryPath)
  | CEEntryAlreadyExists (QualifiedPath EntryPath)
  deriving stock (Show, Generic)

data CreateResult
  = CRSuccess (QualifiedPath Entry)
  | CRCreateError CreateError
  deriving stock (Show)

data SetFieldResult
  = SFRSuccess FieldName (QualifiedPath Entry)
  | SFREntryNotFound (QualifiedPath EntryPath)
  | SFRMissingFieldContents FieldName (QualifiedPath EntryPath)
  deriving stock (Show)

data DeleteFieldResult
  = DFRSuccess FieldName (QualifiedPath Entry)
  | DFREntryNotFound (QualifiedPath EntryPath)
  | DFRFieldNotFound FieldName
  deriving stock (Show)

type RenameResult = CopyResult

data CopyResult
  = CPRSuccess Bool [(QualifiedPath EntryPath, QualifiedPath EntryPath)]
  | CPRPathNotFound (QualifiedPath Path)
  | CPRMissingEntryName
  | CPRSamePath (QualifiedPath Path)
  | CPRCreateErrors [(QualifiedPath EntryPath, CreateError)]
  deriving stock (Show)

data DeleteResult
  = DRSuccess Bool [QualifiedPath EntryPath]
  | DRPathNotFound (QualifiedPath Path)
  | DRDirectoryFound (QualifiedPath Path)
  deriving stock (Show)

data TagResult
  = TRSuccess (QualifiedPath Entry) EntryTag Bool
  | TREntryNotFound (QualifiedPath EntryPath)
  | TRTagNotFound EntryTag
  | TRDuplicateTag EntryTag
  deriving stock (Show)

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data ViewOptions = ViewOptions
  { voQPath :: QualifiedPath Path
  , voFieldName :: Maybe FieldName
  }
  deriving stock (Show)

data CreateOptions = CreateOptions
  { coQPath :: QualifiedPath EntryPath
  , coEdit :: Bool
  , coForce :: Bool
  , coTags :: Set EntryTag
  , coFields :: [FieldInfo]
  , coPrivateFields :: [FieldInfo]
  }
  deriving stock (Show)

data SetFieldOptions = SetFieldOptions
  { sfoQPath :: QualifiedPath EntryPath
  , sfoFieldName :: FieldName
  , sfoFieldContents :: Maybe FieldContents
  , sfoVisibility :: Maybe FieldVisibility
  }
  deriving stock (Show)

data DeleteFieldOptions = DeleteFieldOptions
  { dfoQPath :: QualifiedPath EntryPath
  , dfoFieldName :: FieldName
  }
  deriving stock (Show)

data FindOptions = FindOptions
  { foQPath :: Maybe (QualifiedPath Path)
  , foText :: Maybe Text
  , foSort :: Maybe (Sort, Direction)
  , foFilters :: [Filter]
  }
  deriving stock (Show)

data RenameOptions = RenameOptions
  { roDryRun :: Bool
  , roQOldPath :: QualifiedPath Path
  , roQNewPath :: QualifiedPath Path
  , roForce :: Bool
  }
  deriving stock (Show)

data CopyOptions = CopyOptions
  { cpoDryRun :: Bool
  , cpoQOldPath :: QualifiedPath Path
  , cpoQNewPath :: QualifiedPath Path
  , cpoForce :: Bool
  }
  deriving stock (Show)

data DeleteOptions = DeleteOptions
  { doDryRun :: Bool
  , doQPath :: QualifiedPath Path
  , doRecursive :: Bool
  }
  deriving stock (Show)

data TagOptions = TagOptions
  { toQPath :: QualifiedPath EntryPath
  , toTagName :: EntryTag
  , toDelete :: Bool
  }
  deriving stock (Show)

----------------------------------------------------------------------------
-- Option arguments
----------------------------------------------------------------------------

data FieldInfo = FieldInfo
  { fiName :: FieldName
  , fiContents :: FieldContents
  }
  deriving stock (Show)

data Direction = Asc | Desc
  deriving stock (Show)

data Sort
  = SortByEntryName
  | SortByEntryDate
  | SortByFieldContents FieldName
  | SortByFieldDate FieldName
  deriving stock (Show)

data FilterOp = OpGT | OpGTE | OpLT | OpLTE | OpEQ
  deriving stock (Show)

data FilterDate
  = FDYear Year
  | FDMonth Month
  | FDDay Day
  | FDTime UTCTime
  deriving stock (Show)

data Filter
  = FilterByDate FilterOp FilterDate
  | FilterByName Text
  | FilterByField FieldName FilterField
  deriving stock (Show)

data FilterField
  = FilterFieldByDate FilterOp FilterDate
  | FilterFieldByContents Text
  deriving stock (Show)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance FromHttpApiData Filter where
  parseUrlPiece = toServantParser parseFilter

instance FromHttpApiData (Sort, Direction) where
  parseUrlPiece = toServantParser parseSort

----------------------------------------------------------------------------
-- Megaparsec
----------------------------------------------------------------------------

parseFilterOp :: MParser FilterOp
parseFilterOp =
  P.choice @[]
    [ P.string ">=" $> OpGTE
    , P.string "<=" $> OpLTE
    , P.char '>' $> OpGT
    , P.char '<' $> OpLT
    , P.char '=' $> OpEQ
    ]

instance ToParamSchema (Sort, Direction) where
  toParamSchema _ =
    mempty
      & format ?~ sortDirectionFormat
      & example ?~ "name:asc"
    where
      sortDirectionFormat = T.unlines
        [ "name:<direction>"
        , "date:<direction>"
        , "<direction>=[asc, desc]"
        ]

instance ToParamSchema Filter where
  toParamSchema _ =
    mempty
      & format ?~ filterFormat
      & example ?~ "name~google"
    where
      filterFormat = T.unlines
        [ "name~<substring>"
        , "date<op><date>"
        , "<op>=[>=, <=, >, <, =]"
        , "<date>=['YYYY', 'YYYY-MM', 'YYYY-MM-DD', 'YYYY-MM-DD HH:MM:SS']"
        ]

parseFilter :: MParser Filter
parseFilter =
  P.try parseFilterByName <|> P.try parseFilterByDate <|> parseFilterByField
  where
    parseFilterByName = do
      void $ P.string "name" >> P.char '~'
      rest <- P.takeRest
      guard (not $ T.null rest)
      pure $ FilterByName rest
    parseFilterByDate = do
      void $ P.string "date"
      op <- parseFilterOp
      localTime <- parseFilterDate
      pure $ FilterByDate op localTime

parseFilterByField :: MParser Filter
parseFilterByField = do
  fieldName <- parseFieldNameWhile (/= ':')
  void $ P.char ':'
  filterField <- parseFilterFieldByContents <|> parseFilterFieldByDate
  pure $ FilterByField fieldName filterField
  where
    parseFilterFieldByContents = do
      void $ P.string "contents" >> P.char '~'
      rest <- P.takeRest
      guard (not $ T.null rest)
      pure $ FilterFieldByContents rest
    parseFilterFieldByDate = do
      op <- P.string "date" >> parseFilterOp
      localTime <- parseFilterDate
      pure $ FilterFieldByDate op localTime

parseFieldInfo :: MParser FieldInfo
parseFieldInfo = do
  fieldName <- parseFieldNameWhile \c -> c /= '=' && not (Char.isSpace c)
  P.hspace >> P.char '=' >> P.hspace
  fieldContents <- parseFieldContentsEof
  pure $ FieldInfo fieldName fieldContents

parseFieldNameWhile :: (Char -> Bool) -> MParser FieldName
parseFieldNameWhile whileCond = do
  fieldName <- P.takeWhile1P (Just "fieldname") whileCond
  either fail pure $ readFieldName' fieldName

-- | Parse the rest of the input as a field content.
parseFieldContentsEof :: MParser FieldContents
parseFieldContentsEof = FieldContents . T.pack <$> P.manyTill P.anySingle P.eof

parseSort :: MParser (Sort, Direction)
parseSort = do
  sort <- parseSortMeans
  void $ P.char ':'
  direction <- parseSortDirection
  return (sort, direction)

parseSortDirection :: MParser Direction
parseSortDirection =
      P.string "asc" $> Asc
  <|> P.string "desc" $> Desc

parseSortMeans :: MParser Sort
parseSortMeans =
  P.try parseSortMeansByFieldContentsOrDate
  <|> parseSortMeansByNameOrDate

parseSortMeansByFieldContentsOrDate :: MParser Sort
parseSortMeansByFieldContentsOrDate = do
  fieldName <- parseFieldNameWhile (/= ':')
  void $ P.char ':'
  P.choice @[] $
    [ (SortByFieldDate fieldName)     <$ (P.string "date")
    , (SortByFieldContents fieldName) <$ (P.string "contents")
    ]

parseSortMeansByNameOrDate :: MParser Sort
parseSortMeansByNameOrDate = P.choice @[] $
  [ SortByEntryName <$ P.string "name"
  , SortByEntryDate <$ (P.string "date")
  ]


----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

readFieldName :: ReadM FieldName
readFieldName = str >>= toReader . readFieldName'

readFieldName' :: Text -> Either String FieldName
readFieldName' input = do
  case newFieldName input of
    Right tag -> pure tag
    Left err -> Left $ unlines
      [ "Invalid field name: " <> show input <> "."
      , pretty err
      ]

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

toReader :: Either String a -> ReadM a
toReader = either readerError pure

toServantParser :: MParser a -> Text -> Either Text a
toServantParser p = first (T.pack . P.errorBundlePretty) . P.parse p "<url>"

-- | Parses any of these formats:
--
-- * @YYYY@
-- * @YYYY-MM@
-- * @YYYY-MM-DD@
-- * @YYYY-MM-DD HH:MM:SS@
parseFilterDate :: MParser FilterDate
parseFilterDate = do
  y <- P.decimal
  optional (P.char '-') >>= \case
    Nothing -> pure $ FDYear y
    Just _ -> do
      m <- twoDigits
      optional (P.char '-') >>= \case
        Nothing ->
          case fromYearMonthValid y m of
            Nothing -> fail "invalid year/month"
            Just month -> pure $ FDMonth month
        Just _ -> do
          d <- twoDigits
          case fromGregorianValid y m d of
            Nothing -> fail "invalid year/month/day"
            Just day ->
              optional (P.char ' ') >>= \case
                Nothing -> pure $ FDDay day
                Just _ -> do
                  hh <- twoDigits <* P.char ':'
                  mm <- twoDigits <* P.char ':'
                  ss <- twoDigits
                  case makeTimeOfDayValid hh mm (fromIntegral @Int @Pico ss) of
                    Nothing -> fail "invalid hour/minute/seconds"
                    Just timeOfDay -> pure $ FDTime (localTimeToUTC utc $ LocalTime day timeOfDay)
  where
    -- | Parse a two-digit integer (e.g. day of month, hour).
    twoDigits :: MParser Int
    twoDigits = do
      a <- P.digitChar
      b <- P.digitChar
      pure $ Char.digitToInt a * 10 + Char.digitToInt b

fieldName :: MParser Text
fieldName = fst <$> P.match (some $ P.noneOf [':'])
