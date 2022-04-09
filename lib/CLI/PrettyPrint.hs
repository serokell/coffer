-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.PrettyPrint where

import Coffer.Directory (Directory(..))
import Coffer.Path (entryPathName)
import Control.Lens
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy qualified as TL
import Data.Time
import Entry
import Fmt

buildDirectory :: Directory -> Builder
buildDirectory = go ""
  where
    go :: Text -> Directory -> Builder
    go dirName (Directory entries subdirs) =
      unlinesF $
        (build dirName <> "/")
        : (entries <&> buildEntry <&> indentF 2)
        <> (subdirs
              & HashMap.mapKeys pretty
              & HashMap.mapWithKey go
              & HashMap.elems
              <&> indentF 2
            )

buildEntry :: Entry -> Builder
buildEntry e =
  unlinesF $ buildEntryHeader e : formattedFields
  where
    formattedFields =
      indentF 2 <$> buildFields (e ^. fields . to HashMap.toList)

buildEntryHeader :: Entry -> Builder
buildEntryHeader e =
  unwordsF $ catMaybes
    [ Just $ build (e ^. path . to entryPathName)
    , Just "-"
    , buildTags (e ^. tags)
    , Just $ buildDate (e ^. dateModified)
    ]

buildTags :: Set EntryTag -> Maybe Builder
buildTags tags =
  if null tags
    then Nothing
    else Just $ listF $ Set.toAscList tags

buildFields :: [(FieldKey,  Field)] -> [Builder]
buildFields fields = do
  let formattedFields = fields <&> buildField
  let maxFieldLength = formattedFields <&> (\(firstLine, _) -> TL.length (toLazyText firstLine) & fromIntegral @Int64 @Int) & maximum

  formattedFields `zip` fields <&> \((firstLine, otherLinesMb), (_, field)) -> do
    let formattedFirstLine = padRightF maxFieldLength ' ' firstLine <> " " <> buildDate (field ^. dateModified)
    case otherLinesMb of
      Nothing -> formattedFirstLine
      Just otherLines -> unlinesF [formattedFirstLine, otherLines]

-- | If the field contents does not contain newline characters,
-- then the field name and content will be displayed in one single line.
-- Otherwise, they'll be displayed on separate lines.
--
-- This function returns a tuple with the builder for the first line
-- and an optional builder for the remaining lines.
buildField :: (FieldKey, Field) -> (Builder, Maybe Builder)
buildField (fk, field) = do
  let fkText = getFieldKey fk
  if T.isInfixOf "\n" (field ^. value . fieldValue)
    then
      ( build fkText <> ":"
      , Just $ indentF 2 $ build $ field ^. value
      )
    else
      ( build fkText <> ": " <> build (field ^. value)
      , Nothing
      )

-- | Prints a `UTCTime` as a readable string with a format similar to: '2022-01-29 16:58:38'.
buildDate :: UTCTime -> Builder
buildDate time =
  "[" <> build (formatTime defaultTimeLocale "%F %T" time) <> "]"
