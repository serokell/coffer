-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.CLI.EditorMode where

import CLI.EditorMode (renderEditorFile)
import CLI.Parser
import CLI.Types (FieldInfo(..))
import Entry (newFieldKey)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Util
import Text.RawString.QQ (r)

hprop_render_parse_roundtrip :: Property
hprop_render_parse_roundtrip = property $ do
  entryPath <- forAll genEntryPath
  publicFields <- forAll $ Gen.list (Range.linear 0 5) genFieldInfo
  privateFields <- forAll $ Gen.list (Range.linear 0 5) genFieldInfo

  let rendered = renderEditorFile entryPath publicFields privateFields
  hparserShouldSucceed parseEditorFile rendered (publicFields, privateFields)

unit_parse_editor_file :: IO ()
unit_parse_editor_file = do
  parserShouldSucceed parseEditorFile
    [r|

# comment1
[Public fields]

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

field5 = âД😱👪日本

[Private fields]

# comment4
privatefield1 = pf1
# comment5
privatefield2 =    pf2

    |]
    ( [ FieldInfo (unsafeFromRight $ newFieldKey "field1") "f1"
      , FieldInfo (unsafeFromRight $ newFieldKey "field2") "f2"
      , FieldInfo (unsafeFromRight $ newFieldKey "field3") ""
      , FieldInfo (unsafeFromRight $ newFieldKey "field4") "first line:\n  second line\n  third line\n"
      , FieldInfo (unsafeFromRight $ newFieldKey "field5") "âД😱👪日本"
      ]
    , [ FieldInfo (unsafeFromRight $ newFieldKey "privatefield1") "pf1"
      , FieldInfo (unsafeFromRight $ newFieldKey "privatefield2") "pf2"

      ]
    )

unit_parses_file_without_trailing_newline :: IO ()
unit_parses_file_without_trailing_newline = do
  parserShouldSucceed parseEditorFile
    [r|[Public fields]
[Private fields]|]
    ( []
    , []
    )

  parserShouldSucceed parseEditorFile
    [r|[Public fields]
[Private fields]
privatefield1=pf1|]
    ( []
    , [ FieldInfo (unsafeFromRight $ newFieldKey "privatefield1") "pf1"
      ]
    )

  parserShouldSucceed parseEditorFile
    [r|[Public fields]
[Private fields]
privatefield1="""
pf1
"""|]
    ( []
    , [ FieldInfo (unsafeFromRight $ newFieldKey "privatefield1") "pf1"
      ]
    )

unit_parse_minimal_editor_file :: IO ()
unit_parse_minimal_editor_file = do
  parserShouldSucceed parseEditorFile
    [r|[Public fields]
[Private fields]|]
    ( []
    , []
    )

unit_fieldname_and_fieldcontents_must_be_on_the_same_line :: IO ()
unit_fieldname_and_fieldcontents_must_be_on_the_same_line = do
  parserShouldFail parseEditorFile
    [r|[Public fields]
name =
  contents
[Private fields]|]

    [r|3:1:
  |
3 |   contents
  | ^^^^^^^^^^^
unexpected "  contents<newline>[Priv"
expecting "[Private fields]" or fieldname
|]

unit_fieldname_must_be_0_indented :: IO ()
unit_fieldname_must_be_0_indented = do
  parserShouldFail parseEditorFile
    [r|[Public fields]
name1 = contents1
  name2 = contents2
[Private fields]|]
    [r|3:1:
  |
3 |   name2 = contents2
  | ^^^^^^^^^^^^^^^^
unexpected "  name2 = conten"
expecting "[Private fields]" or fieldname
|]

  parserShouldFail parseEditorFile
    [r|[Public fields]
  name = contents
[Private fields]|]
    [r|2:1:
  |
2 |   name = contents
  | ^^^^^^^^^^^^^^^^
unexpected "  name = content"
expecting "[Private fields]" or fieldname
|]
