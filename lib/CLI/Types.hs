-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.Types where

import Coffer.Directory (Directory)
import Coffer.Path (EntryPath, Path, QualifiedPath)
import Coffer.Util (MParser)
import Control.Applicative (Alternative(some), optional)
import Control.Monad (guard, void)
import Data.Aeson hiding ((<?>))
import Data.Bifunctor (first)
import Data.Char qualified as Char
import Data.Fixed (Pico)
import Data.Functor (($>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar.Month.Compat (Month, fromYearMonthValid)
import Data.Time.Compat
  (Day, LocalTime(LocalTime), UTCTime, Year, fromGregorianValid, localTimeToUTC, makeTimeOfDayValid,
  utc)
import Entry (Entry, EntryTag, Field, FieldContents, FieldName, FieldVisibility, newFieldName)
import GHC.Generics (Generic)
import Servant (FromHttpApiData(parseUrlPiece))
import Text.Megaparsec
  (MonadParsec(eof, try), choice, errorBundlePretty, match, noneOf, parse, takeRest)
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
  deriving anyclass (ToJSON)

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

-- TODO: don't forget to parse @FilterByField@.
-- Probably would be resolved after fixing (https://github.com/serokell/coffer/issues/89)
instance FromHttpApiData Filter where
  parseUrlPiece = toServantParser do
    choice
      [ do
          void "name~"
          name <- takeRest
          guard $ not $ T.null name
          return $ FilterByName name
      , do
          void "date"
          op <- choice
            [ ">=" $> OpGTE
            , "<=" $> OpLTE
            , ">"  $> OpGT
            , "<"  $> OpLT
            , "="  $> OpEQ
            ]
          FilterByDate op <$> parseFilterDate
      ]

instance FromHttpApiData (Sort, Direction) where
  parseUrlPiece = toServantParser do
    choice
      [ try do
          means <- choice [SortByEntryName <$ "name", SortByEntryDate <$ "date"]
          void ":"
          direction <- choice [Asc <$ "asc", Desc <$ "desc"]
          eof
          return (means, direction)

      , do
          (field, _) <- match $ some $ noneOf [':']
          case newFieldName field of
            Left _ -> fail "field name is incorrect"
            Right field -> do
              void ":"
              means <- choice
                [ SortByFieldContents field <$ "value"
                , SortByFieldDate  field <$ "date"
                ]
              void ":"
              direction <- choice [Asc <$ "asc", Desc <$ "desc"]
              eof
              return (means, direction)
      ]

instance FromHttpApiData (FieldName, FilterField) where
  parseUrlPiece = toServantParser do
    field <- fieldName
    void ":"
    case newFieldName field of
      Left _ -> fail "field name is incorrect"
      Right field -> do
        choice
          [ do
              void "value~"
              value <- takeRest
              guard $ not $ T.null value
              return (field, FilterFieldByContents value)
          , do
              void "date"
              op <- choice
                [ ">=" $> OpGTE
                , "<=" $> OpLTE
                , ">"  $> OpGT
                , "<"  $> OpLT
                , "="  $> OpEQ
                ]
              date <- parseFilterDate
              return (field, FilterFieldByDate op date)
          ]

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

toServantParser :: MParser a -> Text -> Either Text a
toServantParser p = first (T.pack . errorBundlePretty) . parse p "<url>"

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
fieldName = fst <$> match (some $ noneOf [':'])
