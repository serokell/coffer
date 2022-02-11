-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.EditorMode where

import CLI.Parser (parseEditorFile)
import CLI.Types
import Coffer.Path (EntryPath, QualifiedPath(qpPath))
import Control.Lens
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Entry (FieldValue(unFieldValue))
import Entry qualified as E
import Fmt (pretty)
import System.Environment (lookupEnv)
import System.IO (SeekMode(AbsoluteSeek), hFlush, hGetContents, hPutStr, hSeek)
import System.IO.Temp
import System.Process as Process
import Text.Megaparsec (ParseError, ParseErrorBundle, PosState)
import Text.Megaparsec qualified as P

data AnnotatedLine = AnnotatedLine
  { _alLine :: Text
  , _alErrors :: [Text]
  }

makeLenses 'AnnotatedLine

-- TODO: tags

editorFileHeader :: EntryPath -> Text
editorFileHeader path = T.pack
  [i|### Fields for '#{pretty path :: Text}'
###
### Examples:
###
### username = John Doe
### address = """
### 123 Main Street
### Anytown
### """
|]

renderEditorFile :: EntryPath -> [FieldInfo] -> [FieldInfo] -> Text
renderEditorFile path fields privateFields = T.pack
  [i|#{editorFileHeader path}
[Public fields]
#{T.unlines $ displayField <$> fields}
[Private fields]
#{T.unlines $ displayField <$> privateFields}
|]
  where
    displayField field = E.getFieldKey (fiName field) <> " = " <> displayFieldContents (unFieldValue . fiContents $ field)

    displayFieldContents contents =
      -- If the field contents contain newline characters,
      -- display them in multiple lines and wrapped with triple quotes.
      if T.isInfixOf "\n" contents
        then "\"\"\"\n" <> contents <> "\n\"\"\""
        else contents

editorMode :: CreateOptions -> IO CreateOptions
editorMode opts = do
  editorEnvVar <- lookupEnv "EDITOR" <&> fromMaybe "vi"
  let entryPath = (qpPath . coQPath) opts

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

        -- Parse temp file contents.
        case P.parse parseEditorFile fpath editorFileContents' of
          Right (public, private) ->
            pure opts
              { coFields = public
              , coPrivateFields = private
              }
          Left err -> do
            putStrLn "Failed to parse file."
            putStrLn $ P.errorBundlePretty err

            go $ editorFileContents'
              & annotateEditorFile err -- Add annotations for parsing errors
              & removeComments -- Remove parsing errors from previous attempts
              & renderAnnotatedLines
              & mappend (editorFileHeader entryPath)

  go $ renderEditorFile entryPath (coFields opts) (coPrivateFields opts)

-- | Remove all lines that begin with `#`.
removeComments :: [AnnotatedLine] -> [AnnotatedLine]
removeComments als =
  als & filter (\al -> al ^? alLine . _head /= Just '#')

renderAnnotatedLines :: [AnnotatedLine] -> Text
renderAnnotatedLines als =
  als
    <&> (\al -> T.intercalate "\n" (al ^. alLine : al ^. alErrors))
    & T.unlines

{- | For each error in the bunddle, adds a note with the parsing error
next to the offending line. E.g.:

> pw 1234
> #  ^
> # unexpected '1'
> # expecting '=' or white space
-}
annotateEditorFile :: ParseErrorBundle Text Void -> Text -> [AnnotatedLine]
annotateEditorFile bundle fileContents =
  fileContents
    & T.lines
    -- Adding an extra empty line at the end.
    -- If a parsing error occurs at EOF, we can annotate this line.
    & (++ [""])
    <&> mkAnnotatedLine
    & annotateLines bundle
  where
    mkAnnotatedLine :: Text -> AnnotatedLine
    mkAnnotatedLine t = AnnotatedLine t []

    annotateLines :: ParseErrorBundle Text Void -> [AnnotatedLine] -> [AnnotatedLine]
    annotateLines bundle lines =
      fst $
        foldl' annotateLine
          (lines, P.bundlePosState bundle)
          (P.bundleErrors bundle)

    -- | Finds the offending line, and adds one annotation with the parser error.
    annotateLine :: ([AnnotatedLine], PosState Text) -> ParseError Text Void -> ([AnnotatedLine], PosState Text)
    annotateLine (lines, posState) err = (lines', posState')
      where
        (_, posState') = P.reachOffset (P.errorOffset err) posState
        lineNumber = P.unPos (P.sourceLine $ P.pstateSourcePos posState') - 1
        columnNumber = P.unPos (P.sourceColumn $ P.pstateSourcePos posState') - 1
        errMsg =
          err
            & P.parseErrorTextPretty
            & T.pack
            & T.lines
            <&> mappend "# "
        caretLine = "#" <> T.replicate (columnNumber - 1) " " <> "^"
        lines' = lines & ix lineNumber . alErrors <>~ (caretLine : errMsg)
