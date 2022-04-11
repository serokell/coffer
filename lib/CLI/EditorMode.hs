-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.EditorMode where

import CLI.EntryView
import CLI.ParseError
import CLI.Types
import Coffer.Path (EntryPath, QualifiedPath, mkQualifiedEntryPath)
import Control.Lens
import Data.Bifunctor (Bifunctor(first))
import Data.Either (lefts, rights)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.IO (SeekMode(AbsoluteSeek), hFlush, hGetContents, hPutStr, hSeek)
import System.IO.Temp
import System.Process as Process
import Text.Interpolation.Nyan
import Text.Megaparsec qualified as P
import Toml qualified

data AnnotatedLine = AnnotatedLine
  { _alLine :: Text
  , _alErrors :: [Text]
  }

makeLenses 'AnnotatedLine

mkAnnotatedLine :: Text -> AnnotatedLine
mkAnnotatedLine t = AnnotatedLine t []

headerExample :: Text
headerExample = [int|s|
# Example:
#
# path = "/path/to/secret/entry"
# tags = [
#   "first tag",
#   "important"
# ]
#
# [[field]]
# name = "test field"
# private = false
# contents = """
# Some
# multiline
# thing
# """
|]

examplePath :: QualifiedPath EntryPath
examplePath =
  case mkQualifiedEntryPath "/example/path" of
    Right entryPath -> entryPath
    _ -> undefined -- Idk what I should do in this case

renderEditorFile :: CreateOptions -> Text
renderEditorFile opts = Toml.encode entryViewCodec entryView
  where
    publicFields = coFields opts <&> \field -> FieldInfoView field False
    privateFields = coPrivateFields opts <&> \field -> FieldInfoView field True
    entryPath = fromMaybe examplePath (coQPath opts)
    entryView = EntryView entryPath (coTags opts) (publicFields <> privateFields)

setOpts :: CreateOptions -> EntryView -> CreateOptions
setOpts opts entryView = opts
  { coQPath = Just qPath
  , coTags = tags
  , coFields = publicFields
  , coPrivateFields = privateFields
  }
  where
    qPath = entryView ^. qEntryPath
    tags = entryView ^. entryTags
    publicFields = entryView ^.. fields . each . filtered (not . view private) . fieldInfo
    privateFields = entryView ^.. fields . each . filtered (view private) . fieldInfo

editorMode :: CreateOptions -> IO CreateOptions
editorMode opts = do
  editorEnvVar <- lookupEnv "EDITOR" <&> fromMaybe "vi"

  let
    go :: Text -> IO CreateOptions
    go editorFileContents = do
      withSystemTempFile "coffer" \fpath fhandle -> do
        -- Write fields to temp file.
        hPutStr fhandle $ T.unpack editorFileContents
        hFlush fhandle

        -- Launch editor.
        -- Note: The "editor" env variable may contain options/switches (e.g. `code --wait`),
        -- so we have to split those.
        let editorName = editorEnvVar ^?! to words . _head
        let editorArgs = editorEnvVar ^?! to words . _tail <> [fpath]
        putStrLn "Launching editor..."
        Process.callProcess editorName editorArgs

        -- Read temp file.
        hSeek fhandle AbsoluteSeek 0
        editorFileContents' <- T.pack <$> hGetContents fhandle

        case Toml.decode entryViewCodec editorFileContents' of
          Right entryView -> do
            pure $ setOpts opts entryView
          Left errors -> do
            putStrLn "Failed to parse file."
            go $ editorFileContents'
              & annotateEditorFile errors -- Add annotations for parsing errors
              & renderAnnotatedLines
              & T.strip

  go $ headerExample <> "\n\n" <> renderEditorFile opts

renderAnnotatedLines :: [AnnotatedLine] -> Text
renderAnnotatedLines als =
  als
    <&> (\al -> T.intercalate "\n" (al ^. alLine : al ^. alErrors))
    & T.unlines

annotateEditorFile :: [Toml.TomlDecodeError] -> Text -> [AnnotatedLine]
annotateEditorFile errors contents =
  contents
    & T.lines
    -- Adding an extra empty line at the end.
    -- If a parsing error occurs at EOF, we can annotate this line.
    & (++ [""])
    <&> mkAnnotatedLine
    & annotateErrors errors

{- | For each @ParseError@, adds a note with the parsing error
next to the offending line. E.g.:

> pw 1234
> #  ^
> # unexpected '1'
> # expecting '=' or white space
-}
annotateParseErrors :: [ParseError] -> [AnnotatedLine] -> [AnnotatedLine]
annotateParseErrors errors lines = foldl' annotateParseError lines errors
  where
    -- | Finds the offending line, and adds one annotation with the parser error.
    annotateParseError :: [AnnotatedLine] -> ParseError -> [AnnotatedLine]
    annotateParseError lines error = lines & ix (error ^. line - 1) . alErrors <>~ (caretLine : errMsg)
      where
        caretLine = "#" <> T.replicate (error ^. offset - 2) " " <> "^"
        errMsg =
          error ^. errorMessage
            & T.lines
            <&> mappend "# "

annotateOtherErrors :: [Toml.TomlDecodeError] -> [AnnotatedLine] -> [AnnotatedLine]
annotateOtherErrors errors lines = lines <> [AnnotatedLine "" errorLines]
  where
    prettifiedErrors = Toml.prettyTomlDecodeErrors errors
    errorLines
      | null errors = []
      | otherwise =
          prettifiedErrors
            & T.lines
            <&> mappend "# "

annotateErrors :: [Toml.TomlDecodeError] -> [AnnotatedLine] -> [AnnotatedLine]
annotateErrors errors lines =
  lines
    & annotateParseErrors parseErrors
    & annotateOtherErrors otherErrors
  where
    parseAndOtherErrors :: [Either Toml.TomlDecodeError ParseError]
    parseAndOtherErrors =
      flip map errors \case
        parseErr@(Toml.ParseError (Toml.TomlParseError err)) ->
          P.parse (parseParseError <* P.eof) "" err & first (const parseErr)
        otherErr -> Left otherErr

    parseErrors :: [ParseError]
    parseErrors = rights parseAndOtherErrors

    otherErrors :: [Toml.TomlDecodeError]
    otherErrors = lefts parseAndOtherErrors
