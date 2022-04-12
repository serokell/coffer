-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE OverloadedLists #-}

module CLI.Parser
  ( parserInfo
  ) where

import Options.Applicative
import qualified Options.Applicative.Help.Pretty as Pretty
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Functor ((<&>), ($>))
import qualified Data.List as List
import Data.Function ((&))
import Control.Arrow ((>>>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Data.Void ( Void )
import Data.Foldable (asum)
import Data.Time.Compat (LocalTime (..), makeTimeOfDayValid, localTimeToUTC, utc)
import Data.Time.Calendar.Compat ( fromGregorianValid )
import Data.Time.Calendar.Month.Compat (fromYearMonthValid)
import Control.Monad (guard, void)
import qualified Data.Char as Char
import Data.Fixed (Pico)
import Data.Bifunctor (first)
import qualified Data.Set as Set

import CLI.Types
import Entry (FieldKey, EntryTag, newEntryTag, newFieldKey, FieldVisibility (Public, Private), FieldValue (FieldValue))
import Coffer.Path (Path, mkPath, EntryPath, mkEntryPath, QualifiedPath (QualifiedPath))
import BackendName (newBackendName, BackendName)

{-# ANN module ("HLint: ignore Use <$>" :: Text) #-}

parserInfo :: ParserInfo Options
parserInfo =
  info (parser <**> helper) $
    fullDesc
    <> progDesc "TODO: coffer description goes here"
    <> header "TODO: coffer description goes here"

parser :: Parser Options
parser = Options
  <$> configPathParser
  <*> commandParser
  where
    configPathParser :: Parser (Maybe FilePath)
    configPathParser = optional $
      option str $ mconcat
        [ long "config"
        , short 'c'
        , metavar "CONFIG"
        , help $ unlines
            [ "Specify config file path."
            , "When this option is not set, the 'COFFER_CONFIG' environment variable will be used."
            , "When neither is set, it will default to 'config.toml'."
            ]
        ]

commandParser :: Parser SomeCommand
commandParser =
  subparser (mconcat
    [ mkCommand "view" CmdView viewOptions
        "View entries under the specified path, optionally returning only the specified field for each entry"
    , mkCommand "create" CmdCreate createOptions
        "Create a new entry at the specified path"
    , mkCommand "set-field" CmdSetField setFieldOptions
        "Set a field on the entry at the specified path"
    , mkCommand "delete-field" CmdDeleteField deleteFieldOptions
        "Delete a field from the entry at the specified path"
    , mkCommand "find" CmdFind findOptions
        "Find and list entries, optionally filtering"
    , mkCommand "rename" CmdRename renameOptions
        "Rename an entry or directory"
    , mkCommand "copy" CmdCopy copyOptions
        "Copy an entry or directory"
    , mkCommand "delete" CmdDelete deleteOptions
        "Delete an entry or directory"
    , mkCommand "tag" CmdTag tagOptions
        "Add or remove tags from an entry"
    ]) <|> defaultCommandParser
  where
    mkCommand :: String -> (opts -> Command res) -> Parser opts -> String -> Mod CommandFields SomeCommand
    mkCommand cmdName constructor optsParser cmdHelp =
      command cmdName $
        info (helper <*> (SomeCommand . constructor <$> optsParser)) $
        progDesc cmdHelp

    defaultCommandParser :: Parser SomeCommand
    defaultCommandParser = SomeCommand . CmdView <$> viewOptions

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

viewOptions :: Parser ViewOptions
viewOptions = do
  ViewOptions
    <$> argument readQualifiedPath ( mconcat
          [ metavar "PATH"
          , help "The path to either a directory of entries, or a single entry"
          ])
    <*> optional
          ( argument readFieldKey $ mconcat
              [ metavar "FIELDNAME"
              , help "The optional field name to fetch the contents of"
              ]
          )

createOptions :: Parser CreateOptions
createOptions =
  CreateOptions
    <$> argument readQualifiedEntryPath ( mconcat
          [ metavar "ENTRYPATH"
          , help "The path to insert the new entry into, this must not already be a directory or an entry unless `-f` is specified"
          ])
    <*> switch ( mconcat
          [ long "edit"
          , short 'e'
          , help $ unlines
              [ "Open an editor to configure the fields and tags for this entry."
              , "By default, `vi` will be used."
              , "This can be set using the `$EDITOR` environment variable."
              ]
          ])
    <*> switch ( mconcat
          [ long "force"
          , short 'f'
          , help "If a directory or entry already exists under the specified path, delete it and insert this entry instead"
          ])
    <*> (Set.fromList <$> many ( option readEntryTag $ mconcat
          [ long "tag"
          , metavar "TAG"
          , help "Tag to add to this new entry, this may be specified multiple times"
          ])
        )
    <*> many ( option readFieldInfo $ mconcat
          [ long "field"
          , metavar "NAME=CONTENT"
          , help "A field to insert into the new entry, with the format 'fieldname=fieldcontents', this may be specified multiple times"
          ])
    <*> many ( option readFieldInfo $ mconcat
          [ long "privatefield"
          , metavar "NAME=CONTENT"
          , help $ unlines
              [ "Same as `--field`, but the field will be marked as private."
              , "Private fields can only be viewed with 'coffer view',"
              , "and will be hidden when using other commands."
              ]
          ])

setFieldOptions :: Parser SetFieldOptions
setFieldOptions =
  SetFieldOptions
    <$> argument readQualifiedEntryPath ( mconcat
          [ metavar "ENTRYPATH"
          , help "The path to set the field value on, this must already exist as an entry"
          ])
    <*> argument readFieldKey ( mconcat
          [ metavar "FIELDNAME"
          , help "The name of the field to set"
          ])
    <*> optional (argument readFieldValue $ mconcat
          [ metavar "FIELDCONTENTS"
          , help "The contents to insert into the field. Required when creating a new field, optional otherwise"
          ])
    <*> optional (option readFieldVisibility $ mconcat
          [ long "visibility"
          , short 'V'
          , metavar "VISIBILITY"
          , help $ unlines
              [ "Whether to mark this field as 'public' or 'private'"
              , "New fields are public by default when this option is omitted."
              , "Private fields can only be viewed with 'coffer view',"
              , "and will be hidden when using other commands."
              ]
          ])

deleteFieldOptions :: Parser DeleteFieldOptions
deleteFieldOptions =
  DeleteFieldOptions
    <$> argument readQualifiedEntryPath ( mconcat
          [ metavar "ENTRYPATH"
          , help "The path to the entry with the field to delete"
          ])
    <*> argument readFieldKey ( mconcat
          [ metavar "FIELDNAME"
          , help "The name of the field to delete"
          ])

findOptions :: Parser FindOptions
findOptions =
  FindOptions
    <$> optional (argument readQualifiedPath $ mconcat
          [ metavar "PATH"
          , help "If specified, only show entries within this path (use `/` to find everything)"
          ])
    <*> optional (argument str $ mconcat
          [ metavar "TEXT"
          , help "The text to search for in the paths and tags of entries"
          ])
    <*> optional (option readSort $ mconcat
          [ metavar "SORT"
          , long "sort"
          , helpDoc $ Just $ Pretty.vsep
            [ "Sort the entries inside each directory."
            , expectedSortFormat
            ]
          ])
    <*> many (option readFilter $ mconcat
          [ metavar "FILTER"
          , long "filter"
          , helpDoc $ Just $ Pretty.vsep
            [ "Filter entries."
            , expectedFilterFormat
            ]
          ])
    <*> many (option readFilterField $ mconcat
          [ metavar "FILTERFIELD"
          , long "filter-field"
          , helpDoc $ Just $ Pretty.vsep
            [ "Filter entries based on a field."
            , expectedFilterFieldFormat
            ]
          ])

renameOptions :: Parser RenameOptions
renameOptions =
  RenameOptions
    <$> switch ( mconcat
          [ long "dry-run"
          , short 'd'
          , help "Don't actually rename anything, just show what would be done"
          ])
    <*> argument readQualifiedPath ( mconcat
          [ metavar "OLDPATH"
          , help "The path to move the old directory or entry from"
          ])
    <*> argument readQualifiedPath ( mconcat
          [ metavar "NEWPATH"
          , help "The path to move the directory or entry to"
          ])
    <*> switch ( mconcat
          [ long "force"
          , short 'f'
          , help "If a directory or entry already exists under the specified path, delete it and insert this entry instead"
          ])

copyOptions :: Parser CopyOptions
copyOptions =
  CopyOptions
    <$> switch ( mconcat
          [ long "dry-run"
          , short 'd'
          , help "Don't actually copy anything, just show what would be done"
          ])
    <*> argument readQualifiedPath ( mconcat
          [ metavar "OLDPATH"
          , help "The path to copy the old directory or entry from"
          ])
    <*> argument readQualifiedPath ( mconcat
          [ metavar "NEWPATH"
          , help "The path to copy the directory or entry to"
          ])
    <*> switch ( mconcat
          [ long "force"
          , short 'f'
          , help "If a directory or entry already exists under the specified path, delete it and insert this entry instead"
          ])

deleteOptions :: Parser DeleteOptions
deleteOptions =
  DeleteOptions
    <$> switch ( mconcat
          [ long "dry-run"
          , short 'd'
          , help "Don't actually delete anything, just show what would be done"
          ])
    <*> argument readQualifiedPath ( mconcat
          [ metavar "PATH"
          , help "The path to the entry or directory to delete"
          ])
    <*> switch ( mconcat
          [ long "recursive"
          , short 'r'
          , help "If the specified path is a directory, remove it and its contents"
          ])

tagOptions :: Parser TagOptions
tagOptions =
  TagOptions
    <$> argument readQualifiedEntryPath ( mconcat
          [ metavar "ENTRYPATH"
          , help "The path to the entry to add or delete a tag from"
          ])
    <*> argument readEntryTag ( mconcat
          [ metavar "TAGNAME"
          , help "The name of the tag to add or delete"
          ])
    <*> switch ( mconcat
          [ long "delete"
          , short 'd'
          , help "Remove this tag instead of setting it"
          ])

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

readPath' :: Text -> Either String Path
readPath' input =
  mkPath input & first \err -> unlines
    [ "Invalid path: " <> show input <> "."
    , T.unpack err
    ]

readEntryPath' :: Text -> Either String EntryPath
readEntryPath' input =
  mkEntryPath input & first \err -> unlines
    [ "Invalid entry path: " <> show input <> "."
    , T.unpack err
    ]

_readEntryPath :: ReadM EntryPath
_readEntryPath = str >>= toReader . readEntryPath'

readEntryTag :: ReadM EntryTag
readEntryTag = do
  eitherReader \input ->
    newEntryTag (T.pack input) & first \err -> unlines
      [ "Invalid tag: " <> show input <> "."
      , T.unpack err
      ]

readBackendName' :: Text -> Either String BackendName
readBackendName' input =
  newBackendName input & first \err -> unlines
    [ "Invalid backend name: " <> show input <> "."
    , T.unpack err
    ]

readFieldVisibility :: ReadM FieldVisibility
readFieldVisibility =
  eitherReader $ readSum "visibility"
    [ ("public", Public)
    , ("private", Private)
    ]

readFieldKey :: ReadM FieldKey
readFieldKey = str >>= toReader . readFieldKey'

readFieldKey' :: Text -> Either String FieldKey
readFieldKey' input = do
  case newFieldKey input of
    Right tag -> pure tag
    Left err -> Left $ unlines
      [ "Invalid field name: " <> show input <> "."
      , T.unpack err
      ]

readQualifiedEntryPath :: ReadM (QualifiedPath EntryPath)
readQualifiedEntryPath = do
  eitherReader \input ->
    case T.splitOn "#" (T.pack input) of
      [backendNameStr, entryPathStr] -> do
        backendName <- readBackendName' backendNameStr
        entryPath <- readEntryPath' entryPathStr
        pure $ QualifiedPath (Just backendName) entryPath
      [entryPathStr] -> do
        entryPath <- readEntryPath' entryPathStr
        pure $ QualifiedPath Nothing entryPath
      _ ->
        Left $ unlines
                [ "Invalid qualified entry path format: " <> show input <> "."
                , show expectedQualifiedEntryPathFormat
                ]

readQualifiedPath :: ReadM (QualifiedPath Path)
readQualifiedPath = do
  eitherReader \input ->
    case T.splitOn "#" (T.pack input) of
      [backendNameStr, pathStr] -> do
        backendName <- readBackendName' backendNameStr
        path <- readPath' pathStr
        pure $ QualifiedPath (Just backendName) path
      [pathStr] -> do
        path <- readPath' pathStr
        pure $ QualifiedPath Nothing path
      _ ->
        Left $ unlines
                [ "Invalid qualified path format: " <> show input <> "."
                , show expectedQualifiedPathFormat
                ]

readFieldValue :: ReadM FieldValue
readFieldValue = str <&> FieldValue

readFieldInfo :: ReadM FieldInfo
readFieldInfo = do
  eitherReader \input ->
    P.parse (parseFieldInfo <* P.eof) "" (T.pack input) & first \err -> unlines
      [ "Invalid field format: " <> show input <> "."
      , "Expected format: 'fieldname=fieldcontents'."
      , ""
      , "Parser error:"
      , P.errorBundlePretty err
      ]

readSort :: ReadM (Sort, Direction)
readSort = do
  eitherReader \input ->
    case T.splitOn ":" (T.pack input) of
      [means, direction] -> do
        direction' <- readDirection direction
        case means of
          "name" -> pure (SortByEntryName, direction')
          "date" -> pure (SortByEntryDate, direction')
          _ -> Left $ unlines
            [ "Invalid sort: " <> show means <> "."
            , "Choose one of: 'name', 'date'."
            , ""
            , show expectedSortFormat
            ]
      [fieldName, means, direction] -> do
        fieldName' <- readFieldKey' fieldName
        direction' <- readDirection direction
        case means of
          "value" -> pure (SortByFieldValue fieldName', direction')
          "date" -> pure (SortByFieldDate fieldName', direction')
          _ -> Left $ unlines
            [ "Invalid sort: " <> show means <> "."
            , "Choose one of: 'value', 'date'."
            , ""
            , show expectedSortFormat
            ]
      _ -> Left $ unlines
        [ "Invalid sort format: " <> show input <> "."
        , show expectedSortFormat
        ]

expectedSortFormat :: Pretty.Doc
expectedSortFormat = Pretty.vsep
  [ "Expected format is:"
  , " * to sort by entry name: 'name:<direction>',"
  , " * to sort by the entry's last modified date: 'date:<direction>',"
  , " * to sort by a field's value: '<fieldname>:value:<direction>',"
  , " * to sort by a field's last modified date: '<fieldname>:date:<direction>'."
  , "Direction can be 'asc' or 'desc'."
  , "Examples: 'name:desc', 'password:date:asc'."
  ]

readDirection :: Text -> Either String Direction
readDirection =
  T.unpack >>> readSum "direction"
    [ ("asc", Asc)
    , ("desc", Desc)
    ]

readFilter :: ReadM Filter
readFilter = do
  eitherReader \input ->
    P.parse (parseFilter <* P.eof) "" (T.pack input) & first \err -> unlines
      [ "Invalid filter format: " <> show input <> "."
      , show expectedFilterFormat
      , ""
      , "Parser error:"
      , P.errorBundlePretty err
      ]

expectedQualifiedEntryPathFormat :: Pretty.Doc
expectedQualifiedEntryPathFormat = Pretty.vsep
  [ "Expected format is: [<backend-name>#]<entry-path>."
  , "<backend-name> can be a string of the following characters: [a-zA-Z0-9] and symbols '-', '_', ';'."
  , "Examples: 'vault_kv-backend#secrets/google', 'my/passwords/entry'."
  ]

expectedQualifiedPathFormat :: Pretty.Doc
expectedQualifiedPathFormat = Pretty.vsep
  [ "Expected format is: [<backend-name>#]<path>."
  , "<backend-name> can be a string of the following characters: [a-zA-Z0-9] and symbols '-', '_', ';'."
  , "Examples: 'vault_kv-backend#secrets/google', 'my/passwords/mypage/'."
  ]

expectedFilterFormat :: Pretty.Doc
expectedFilterFormat = Pretty.vsep
  [ "Expected format is: 'name~<substring>' or `date<op><date>`."
  , "<op> can be '<=', '>=', '<', '>', or '='."
  , "<date> can be 'YYYY', 'YYYY-MM', 'YYYY-MM-DD', or 'YYYY-MM-DD HH:MM:SS'."
  , "Examples: 'name~vault', 'date<2020-02'."
  ]

readFilterField :: ReadM (FieldKey, FilterField)
readFilterField = do
  eitherReader \input ->
    P.parse (parseFilterField <* P.eof) "" (T.pack input) & first \err -> unlines
      [ "Invalid filter-field format: " <> show input <> "."
      , show expectedFilterFieldFormat
      , ""
      , "Parser error:"
      , P.errorBundlePretty err
      ]

expectedFilterFieldFormat :: Pretty.Doc
expectedFilterFieldFormat = Pretty.vsep
  [ "Expected format is: '<fieldname>:value~<substring>' or `<fieldname>:date<op><date>`."
  , "<op> can be '<=', '>=', '<', '>', or '='."
  , "<date> can be 'YYYY', 'YYYY-MM', 'YYYY-MM-DD', or 'YYYY-MM-DD HH:MM:SS'."
  , "Examples: 'url:value~google.com', 'pw:date<2020-02'."
  ]

----------------------------------------------------------------------------
-- Megaparsec
----------------------------------------------------------------------------

type MParser = P.Parsec Void Text

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

parseFilterOp :: MParser FilterOp
parseFilterOp =
  asum @[]
    [ P.string ">=" $> OpGTE
    , P.string "<=" $> OpLTE
    , P.char '>' $> OpGT
    , P.char '<' $> OpLT
    , P.char '=' $> OpEQ
    ]

parseFilter :: MParser Filter
parseFilter =
  parseFilterByName <|> parseFilterByDate
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

parseFilterField :: MParser (FieldKey, FilterField)
parseFilterField = do
  fieldName <- parseFieldNameWhile (/= ':')
  void $ P.char ':'
  filterField <- parseFilterFieldByValue <|> parseFilterFieldByDate

  pure (fieldName, filterField)
  where
    parseFilterFieldByValue = do
      void $ P.string "value" >> P.char '~'
      rest <- P.takeRest
      guard (not $ T.null rest)
      pure $ FilterFieldByValue rest
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

parseFieldNameWhile :: (Char -> Bool) -> MParser FieldKey
parseFieldNameWhile whileCond = do
  fieldName <- P.takeWhile1P (Just "fieldname") whileCond
  either fail pure $ readFieldKey' fieldName

-- | Parse the rest of the input as a field content.
parseFieldContentsEof :: MParser FieldValue
parseFieldContentsEof = FieldValue . T.pack <$> P.manyTill P.anySingle P.eof

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

toReader :: Either String a -> ReadM a
toReader = either readerError pure

readSum ::  String -> Map String a -> String -> Either String a
readSum sumDescription constructors input =
  case M.lookup input constructors of
    Just cons -> Right cons
    Nothing ->
      Left $ unlines
        [ "Invalid " <> sumDescription <> ": '" <> input <> "'."
        , "Choose one of: " <> constructorNames <> "."
        ]
  where
    constructorNames :: String
    constructorNames =
      constructors
        & M.keys
        <&> (\name -> "'" <> name <> "'")
        & List.intersperse ", "
        & mconcat
