-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.CLI.EditorMode where

import CLI.EditorMode (renderEditorFile)
import CLI.EntryView (EntryView(EntryView), FieldInfoView(FieldInfoView), parseEntryView)
import CLI.Types (CreateOptions(CreateOptions), FieldInfo(..))
import Coffer.Path (mkQualifiedEntryPath)
import Data.Functor ((<&>))
import Data.Set qualified as S
import Entry (FieldValue(FieldValue), newEntryTag, newFieldKey)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Util
import Text.RawString.QQ (r)

hprop_render_parse_roundtrip :: Property
hprop_render_parse_roundtrip = property $ do
  qEntryPath <- forAll genQualifiedEntryPath
  publicFields <- forAll $ Gen.list (Range.linear 0 5) genFieldInfo
  privateFields <- forAll $ Gen.list (Range.linear 0 5) genFieldInfo
  entryTags <- forAll $ Gen.set (Range.linear 0 5) genEntryTag

  let opts = CreateOptions (Just qEntryPath) True False entryTags publicFields privateFields
  let rendered = renderEditorFile opts

  let publicFieldInfoViews = publicFields <&> \field -> FieldInfoView field False
  let privateFieldInfoViews = privateFields <&> \field -> FieldInfoView field True

  let entryView = EntryView (Just qEntryPath) (publicFieldInfoViews <> privateFieldInfoViews) entryTags

  hparserShouldSucceed parseEntryView rendered entryView

unit_parse_editor_file :: IO ()
unit_parse_editor_file = do
  parserShouldSucceed parseEntryView
    [r|
path = /entry/path

# comment1
[fields]

# comment2
field1 = f1

# comment3
field2=f2

field3 =

field4 = """
first line:
  second line
  third line

"""

field5 = âПривет😱👪日本🤔🤔
# comment4
privatefield1 =~ pf1
# comment5
privatefield2 =~    pf2

[tags]
tag1
important

    |]
    ( EntryView
        (Just (unsafeFromRight $ mkQualifiedEntryPath "/entry/path"))
        [ FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "field1") (FieldValue "f1")) False
        , FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "field2") (FieldValue "f2")) False
        , FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "field3") (FieldValue "")) False
        , FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "field4") (FieldValue "first line:\n  second line\n  third line\n")) False
        , FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "field5") (FieldValue "âПривет😱👪日本🤔🤔")) False
        , FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "privatefield1") (FieldValue "pf1")) True
        , FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "privatefield2") (FieldValue "pf2")) True
        ]
        ( S.fromList
            [ unsafeFromRight $ newEntryTag "tag1"
            , unsafeFromRight $ newEntryTag "important"
            ]
        )
    )

unit_parses_file_without_trailing_newline :: IO ()
unit_parses_file_without_trailing_newline = do
  parserShouldSucceed parseEntryView
    [r|path = /path
[fields]
[tags]|]
    ( EntryView
        (Just (unsafeFromRight $ mkQualifiedEntryPath "/path"))
        []
        S.empty
    )

  parserShouldSucceed parseEntryView
    [r|path = /path
[fields]
privatefield1=~pf1
[tags]|]
    ( EntryView
        (Just (unsafeFromRight $ mkQualifiedEntryPath "/path"))
        [FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "privatefield1") (FieldValue "pf1")) True]
        S.empty
    )

  parserShouldSucceed parseEntryView
    [r|path = /path
[fields]
privatefield1=~"""
pf1
"""
[tags]|]
    ( EntryView
        (Just (unsafeFromRight $ mkQualifiedEntryPath "/path"))
        [FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "privatefield1") (FieldValue "pf1")) True]
        S.empty
    )

unit_parse_minimal_editor_file :: IO ()
unit_parse_minimal_editor_file = do
  parserShouldSucceed parseEntryView
    [r|path = /path
[fields]
[tags]|]
    ( EntryView
        (Just (unsafeFromRight $ mkQualifiedEntryPath "/path"))
        []
        S.empty
    )

unit_fieldname_and_fieldcontents_must_be_separated_by_eq_sign :: IO ()
unit_fieldname_and_fieldcontents_must_be_separated_by_eq_sign = do
  parserShouldFail parseEntryView
    [r|path = /path
[fields]
name contents
[tags]|]

    [r|3:6:
  |
3 | name contents
  |      ^^
unexpected "co"
expecting "=~" or '='
|]

unit_fieldname_may_not_be_0_indented :: IO ()
unit_fieldname_may_not_be_0_indented = do
  parserShouldSucceed parseEntryView
    [r|path = /path
[fields]
name1 = contents1
  name2 = contents2
[tags]|]
    ( EntryView
        (Just (unsafeFromRight $ mkQualifiedEntryPath "/path"))
        [ FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "name1") (FieldValue "contents1")) False
        , FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "name2") (FieldValue "contents2")) False
        ]
        S.empty
    )


  parserShouldSucceed parseEntryView
    [r|path = /path
[fields]
  name = contents
[tags]|]
    ( EntryView
        (Just (unsafeFromRight $ mkQualifiedEntryPath "/path"))
        [FieldInfoView (FieldInfo (unsafeFromRight $ newFieldKey "name") (FieldValue "contents")) False]
        S.empty
    )
