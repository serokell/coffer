-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.EditorMode where

import CLI.EntryView
import CLI.Types
import Control.Lens
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Fmt (pretty)
import System.Environment (lookupEnv)
import System.IO (SeekMode(AbsoluteSeek), hFlush, hGetContents, hPutStr, hSeek)
import System.IO.Temp
import System.Process as Process
import Text.Interpolation.Nyan
import Text.Megaparsec (ParseError, ParseErrorBundle, PosState)
import Text.Megaparsec qualified as P

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
# path = backend#/path/to/entry
#
# [fields]
# public-field = public contents
# private-field =~ private contents
# multiline-thing = """
# multiline
# contents
# """
#
# [tags]
# first-tag
# important
|]

renderEditorFile :: CreateOptions -> Text
renderEditorFile opts = pretty entryView
  where
    publicFields = coFields opts <&> \field -> FieldInfoView field False
    privateFields = coPrivateFields opts <&> \field -> FieldInfoView field True
    entryView = EntryView (coQPath opts) (publicFields <> privateFields) (coTags opts)

setOpts :: CreateOptions -> EntryView -> CreateOptions
setOpts opts entryView = opts
  { coQPath = qPath
  , coTags = tags
  , coFields = publicFields
  , coPrivateFields = privateFields
  }
  where
    qPath = entryView ^. mQEntryPath
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

        case P.parse parseEntryView fpath editorFileContents' of
          Right entryView -> do
            pure $ setOpts opts entryView
          Left errors -> do
            putStrLn "Failed to parse file."
            putStrLn $ P.errorBundlePretty errors
            go $ editorFileContents'
              & annotateEditorFile errors -- Add annotations for parsing errors
              & renderAnnotatedLines
              & T.strip

  go $ headerExample <> "\n\n" <> renderEditorFile opts

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
