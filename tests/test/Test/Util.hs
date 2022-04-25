-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Util
  ( unsafeFromRight

  -- * hunit helpers
  , parserShouldSucceed
  , parserShouldFail

  -- * hedgehog helpers
  , hparserShouldSucceed

  -- * hedgehog generators
  , genQualifiedEntryPath
  , genFieldInfo
  , genEntryTag
  ) where

import BackendName (BackendName, backendNameCharSet, newBackendName)
import CLI.Parser
import CLI.Types (FieldInfo(..))
import Coffer.Path
  (EntryPath(..), PathSegment, QualifiedPath(QualifiedPath), mkPathSegment,
  pathSegmentAllowedCharacters)
import Data.Text (Text)
import Entry (EntryTag, FieldKey, FieldValue(FieldValue), keyCharSet, newEntryTag, newFieldKey)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec qualified as P

unsafeFromRight :: (HasCallStack, Show a) => Either a b -> b
unsafeFromRight = \case
  Right b -> b
  Left a -> error $ "Expected Right, got Left: " <> show a

----------------------------------------------------------------------------
-- HUnit helpers
----------------------------------------------------------------------------

parserShouldSucceed :: (HasCallStack, Show a, Eq a) => MParser a -> Text -> a -> IO ()
parserShouldSucceed p input expected =
  case P.parse p "" input of
    Right actual -> actual @?= expected
    Left err -> assertFailure $ unlines
      [ "Failed to parse input."
      , ""
      , errorBundlePretty err
      ]

parserShouldFail :: (HasCallStack, Show a) => MParser a -> Text -> String -> IO ()
parserShouldFail p input expectedErr =
  case P.parse p "" input of
    Right actual -> assertFailure $ unlines
      [ "Expected parser to fail, but it succeeded."
      , ""
      , show actual
      ]
    Left err -> errorBundlePretty err @?= expectedErr

----------------------------------------------------------------------------
-- Hedgehog helpers
----------------------------------------------------------------------------

hparserShouldSucceed :: (HasCallStack, Show a, Eq a, MonadTest m) => MParser a -> Text -> a -> m ()
hparserShouldSucceed p input expected =
  case P.parse p "" input of
    Right actual -> actual === expected
    Left err -> do
      annotate $ unlines
        [ "Failed to parse input."
        , ""
        , errorBundlePretty err
        ]
      failure

genFromCharSet :: Int -> Int -> [Char] -> (Text -> Either Text a) -> Gen a
genFromCharSet from to charSet smartCtor =
  unsafeFromRight . smartCtor <$>
    Gen.text (Range.linear from to) (Gen.element charSet)

----------------------------------------------------------------------------
-- Hedgehog generators
----------------------------------------------------------------------------

genEntryPath :: Gen EntryPath
genEntryPath = EntryPath <$> Gen.nonEmpty (Range.linear 1 3) genPathSegment

genPathSegment :: Gen PathSegment
genPathSegment = genFromCharSet 1 5 pathSegmentAllowedCharacters mkPathSegment

genFieldKey :: Gen FieldKey
genFieldKey = genFromCharSet 1 20 keyCharSet newFieldKey

genMaybeBackendName :: Gen (Maybe BackendName)
genMaybeBackendName = Gen.maybe genBackendName
  where
    genBackendName :: Gen BackendName
    genBackendName = genFromCharSet 1 10 backendNameCharSet newBackendName

genQualifiedEntryPath :: Gen (QualifiedPath EntryPath)
genQualifiedEntryPath =
  QualifiedPath
    <$> genMaybeBackendName
    <*> genEntryPath

genFieldInfo :: Gen FieldInfo
genFieldInfo =
  FieldInfo
    <$> genFieldKey
    <*> genFieldContents
    where
      genFieldContents :: Gen FieldValue
      genFieldContents =
        FieldValue
          <$> Gen.text (Range.linear 0 20)
                (Gen.frequency
                  [ (4, Gen.unicode)
                  , (1, pure '\n')
                  ]
                )

genEntryTag :: Gen EntryTag
genEntryTag = genFromCharSet 1 5 keyCharSet newEntryTag
