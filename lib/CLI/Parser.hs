-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE OverloadedLists #-}

module CLI.Parser
  ( parserInfo
  , parseFilterDate
  ) where

import CLI.Types
import Coffer.Path (EntryPath, Path, QualifiedPath, mkEntryPath, mkPath, mkQualifiedPath)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Entry
import Fmt (pretty)
import Options.Applicative
import Options.Applicative.Help.Pretty qualified as Pretty
import Text.Interpolation.Nyan
import Text.Megaparsec qualified as P

{-# ANN module ("HLint: ignore Use <$>" :: Text) #-}

parserInfo :: ParserInfo Options
parserInfo =
  info (parser <**> helper) $
    fullDesc
    <> progDesc (unlines [ "Coffer CLI is just one frontend of the coffer project."
                         , "Coffer, the project, is a multi-backend, multi-frontend password manager."
                         , "It reaches out to multiple backends, allowing you to manage multiple password stores af it were one. From anywhere!"
                         ])
    <> header "multi-backend, multi-frontend password manager - CLI frontend"

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
        , help [int|s|
            Specify config file path.
            When this option is not set, the 'COFFER_CONFIG' environment variable will be used.
            When neither is set, it will default to 'config.toml'.
          |]
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
          ( argument readFieldName $ mconcat
              [ metavar "FIELDNAME"
              , help "The optional field name to fetch the contents of"
              ]
          )

createOptions :: Parser CreateOptions
createOptions =
  CreateOptions
    <$> argument readQualifiedEntryPath ( mconcat
          [ metavar "ENTRYPATH"
          , help
              "The path to insert the new entry into, this must not already be \
              \a directory or an entry unless `-f` is specified"
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
          , help
              "If a directory or entry already exists under the specified path, \
              \delete it and insert this entry instead"
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
          , help
              "A field to insert into the new entry, with the format 'fieldname=fieldcontents', \
              \this may be specified multiple times"
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
          , help "The path to set the field contents on, this must already exist as an entry"
          ])
    <*> argument readFieldName ( mconcat
          [ metavar "FIELDNAME"
          , help "The name of the field to set"
          ])
    <*> optional (argument readFieldContents $ mconcat
          [ metavar "FIELDCONTENTS"
          , help $ unlines
              [ "The contents to insert into the field."
              , "Required when creating a new field, optional otherwise."
              ]
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
    <*> argument readFieldName ( mconcat
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
          , help
              "If a directory or entry already exists under the specified path, \
              \delete it and insert this entry instead"
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
          , help
              "If a directory or entry already exists under the specified path, \
              \delete it and insert this entry instead"
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

readPath' :: Text -> Either Text Path
readPath' input =
  mkPath input & first \err -> T.pack $ unlines
    [ "Invalid path: " <> show input <> "."
    , T.unpack err
    ]

readEntryPath' :: Text -> Either Text EntryPath
readEntryPath' input =
  mkEntryPath input & first \err -> T.pack $ unlines
    [ "Invalid entry path: " <> show input <> "."
    , T.unpack err
    ]

readEntryTag :: ReadM EntryTag
readEntryTag = do
  eitherReader \input ->
    newEntryTag (T.pack input) & first \err -> unlines
      [ "Invalid tag: " <> show input <> "."
      ,  pretty err
      ]

readFieldVisibility :: ReadM FieldVisibility
readFieldVisibility =
  eitherReader $ readSum "visibility"
    [ ("public", Public)
    , ("private", Private)
    ]

readQualifiedEntryPath :: ReadM (QualifiedPath EntryPath)
readQualifiedEntryPath = eitherTextReader \input ->
   mkQualifiedPath readEntryPath' input & first \err ->
    [int|s|
      Invalid qualified entry path format: #{show input}.
      #{show expectedQualifiedEntryPathFormat}

      Parser error:
      #{err}
    |]

readQualifiedPath :: ReadM (QualifiedPath Path)
readQualifiedPath = eitherTextReader \input ->
  mkQualifiedPath readPath' input & first \err ->
    [int|s|
      Invalid qualified path format: #{show input}.
      #{show expectedQualifiedPathFormat}

      Parser error:
      #{err}
    |]

readFieldContents :: ReadM FieldContents
readFieldContents = str <&> FieldContents

readFieldInfo :: ReadM FieldInfo
readFieldInfo = do
  eitherReader \input ->
    P.parse (parseFieldInfo <* P.eof) "" (T.pack input) & first \err ->
      [int|s|
        Invalid field format: #{show input}.
        Expected format: 'fieldname=fieldcontents'.

        Parser error:
        #{P.errorBundlePretty err}
      |]

readSort :: ReadM (Sort, Direction)
readSort = do
  eitherReader \input ->
    P.parse (parseSort <* P.eof) "" (T.pack input) & first \err ->
      [int|s|
        Invalid sort format: #{show input}.
        #{show expectedSortFormat}

        Parser error:
        #{P.errorBundlePretty err}
      |]

expectedSortFormat :: Pretty.Doc
expectedSortFormat = Pretty.vsep
  [ "Expected format is:"
  , " * to sort by entry name: 'name:<direction>',"
  , " * to sort by the entry's last modified date: 'date:<direction>',"
  , " * to sort by a field's contents: '<fieldname>:contents:<direction>',"
  , " * to sort by a field's last modified date: '<fieldname>:date:<direction>'."
  , "Direction can be 'asc' or 'desc'."
  , "Examples: 'name:desc', 'password:date:asc'."
  ]

readFilter :: ReadM Filter
readFilter = do
  eitherReader \input ->
    P.parse (parseFilter <* P.eof) "" (T.pack input) & first \err ->
      [int|s|
        Invalid filter format: #{show input}.
        #{show expectedFilterFormat}

        Parser error:
        #{P.errorBundlePretty err}
      |]

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
  [ "Expected format is:"
  , " * to filter by entry name: 'name~<substring>',"
  , " * to filter by entry's last modified date: 'date<op><date>',"
  , " * to filter by a field's contents: '<fieldname>:contents~<substring>',"
  , " * to filter by a field's last modified date: '<fieldname>:date<op><date>'."
  , "<op> can be '<=', '>=', '<', '>', or '='."
  , "<date> can be 'YYYY', 'YYYY-MM', 'YYYY-MM-DD', or 'YYYY-MM-DD HH:MM:SS'."
  , "Examples: 'name~vault', 'date<2020-02', 'url:contents~google.com', 'pw:date<2020-02'."
  ]

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

eitherTextReader :: (Text -> Either Text a) -> ReadM a
eitherTextReader eitherR = eitherReader $ (first T.unpack ) . eitherR . T.pack

readSum ::  String -> Map String a -> String -> Either String a
readSum sumDescription constructors input =
  case M.lookup input constructors of
    Just cons -> Right cons
    Nothing ->
      Left [int|s|
        Invalid #{sumDescription}: '#{input}'.
        Choose one of: #{constructorNames}.
      |]
  where
    constructorNames :: String
    constructorNames =
      constructors
        & M.keys
        <&> (\name -> "'" <> name <> "'")
        & List.intersperse ", "
        & mconcat
