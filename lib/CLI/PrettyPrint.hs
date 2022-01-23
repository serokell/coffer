module CLI.PrettyPrint where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import Data.Time
import Data.Maybe (catMaybes)
import qualified Data.List as List
import Fmt

import Entry
import Coffer.Directory ( Directory(..) )
import Coffer.Path (entryPathName)

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

buildTags :: [EntryTag] -> Maybe Builder
buildTags tags =
  if null tags
    then Nothing
    else Just $ listF $ List.sort tags

buildFields :: [(FieldKey,  Field)] -> [Builder]
buildFields fields = do
  let formattedFields = fields <&> buildField
  let maxFieldLength = formattedFields <&> (\(firstLine, _) -> T.length firstLine) & maximum

  formattedFields `zip` fields <&> \((firstLine, otherLinesMb), (_, field)) -> do
    let formattedFirstLine = padRightF maxFieldLength ' ' firstLine <> " " <> buildDate (field ^. dateModified )
    case otherLinesMb of
      Nothing -> formattedFirstLine
      Just otherLines -> unlinesF [formattedFirstLine, otherLines]

-- | If the field contents does not contain newline characters,
-- then the field name and content will be displayed in one single line.
-- Otherwise, they'll be displayed on separate lines.
--
-- This function returns a tuple with the first line and an optional builder for the remaining lines.
buildField :: (FieldKey, Field) -> (Text, Maybe Builder)
buildField (fk, field) = do
  let fkText = getFieldKey fk
  if T.isInfixOf "\n" (field ^. value)
    then
      ( fkText <> ":"
      , Just $ indentF 2 $ build $ field ^. value
      )
    else
      ( fkText <> ": " <> field ^. value
      , Nothing
      )

-- | Prints a `UTCTime` as a readable string with a format similar to: '2022-01-29 16:58:38'.
buildDate :: UTCTime -> Builder
buildDate time =
  "[" <> build (formatTime defaultTimeLocale "%F %T" time) <> "]"
