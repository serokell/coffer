-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Coffer.Path
  ( PathSegment
  , unPathSegment
  , mkPathSegment
  , pathSegmentAllowedCharacters
  , Path(..)
  , isoPath
  , pathSegments
  , mkPath
  , EntryPath(..)
  , isoEntryPath
  , mkEntryPath
  , entryPathName
  , entryPathParentDir
  , entryPathSegments
  , appendEntryName
  , pathAsEntryPath
  , entryPathAsPath
  , replacePathPrefix
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Fmt (Buildable, build, pretty)
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Control.Lens
import qualified Data.List.NonEmpty as NE
import Control.Monad ((>=>))
import qualified Data.List as List

-- $setup
-- >>> import Fmt (pretty, build)
-- >>> import Control.Lens
-- >>> isRight (Right a) = a
-- >>> unsafeMkPath = isRight . mkPath
-- >>> unsafeMkEntryPath = isRight . mkEntryPath
-- >>> unsafeMkPathSegment = isRight . mkPathSegment

newtype PathSegment = UnsafeMkPathSegment { unPathSegment :: Text }
  deriving stock (Show, Eq)
  deriving newtype (Buildable, Hashable)

mkPathSegment :: Text -> Either Text PathSegment
mkPathSegment segment
  | T.null segment =
      Left "Path segments must contain at least 1 character"
  | T.any (`notElem` pathSegmentAllowedCharacters) segment =
      Left $ "Path segments can only contain the following characters: '" <> T.pack pathSegmentAllowedCharacters <> "'"
  | otherwise = Right $ UnsafeMkPathSegment segment

pathSegmentAllowedCharacters :: [Char]
pathSegmentAllowedCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

-- | Path to a directory or an entry.
newtype Path = Path { unPath :: [PathSegment] }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

makeLensesFor [("unPath", "isoPath")] ''Path

-- |
-- >>> pretty @Path @String <$> mkPath "a/b/c"
-- Right "/a/b/c"
instance Buildable Path where
  build (Path segments) = "/" <> build (T.intercalate "/" $ pretty <$> segments)

-- | Retrieve a list of a path's segments.
--
-- >>> pathSegments $ unsafeMkPath "/a/b/c"
-- ["a","b","c"]
pathSegments :: Path -> [Text]
pathSegments p = p ^.. isoPath . each . to unPathSegment

-- | Parses a path to a directory or entry.
--
-- Leading and trailing slashes are optional.
--
-- >>> pretty <$> mkPath "/"
-- Right "/"
-- >>> pretty <$> mkPath ""
-- Right "/"
-- >>> pretty <$> mkPath "/a/"
-- Right "/a"
-- >>> pretty <$> mkPath "a"
-- Right "/a"
-- >>> pretty <$> mkPath "/a/b/c"
-- Right "/a/b/c"
mkPath :: Text -> Either Text Path
mkPath path = do
  let segments = path
        & stripPrefix
        & stripSuffix
        & T.splitOn "/"

  if segments == [""]
    then Right $ Path []
    else Path <$> traverse mkPathSegment segments

  where
    stripPrefix s = fromMaybe s $ T.stripPrefix "/" s
    stripSuffix s = fromMaybe s $ T.stripSuffix "/" s

-- | An entry's full path (directory + entry's name).
newtype EntryPath = EntryPath { unEntryPath :: NonEmpty PathSegment }
  deriving stock (Show, Eq)

makeLensesFor [("unEntryPath", "isoEntryPath")] ''EntryPath

-- |
-- >>> pretty @EntryPath @String <$> mkEntryPath "a/b/c"
-- Right "/a/b/c"
instance Buildable EntryPath where
  build (EntryPath segments) = build $ Path $ NE.toList segments


mkEntryPath :: Text -> Either Text EntryPath
mkEntryPath = mkPath >=> pathAsEntryPath

-- | Focus the name of the entry.
--
-- >>> unsafeMkEntryPath "/a/b/c" & entryPathName
-- "c"
entryPathName :: EntryPath -> Text
entryPathName = view $ isoEntryPath . last1 . to unPathSegment

-- | Focus the path to the directory this entry is in.
--
-- >>> unsafeMkEntryPath "/parent/dir/c" ^. entryPathParentDir . to build
-- "/parent/dir"
--
-- >>> build $ unsafeMkEntryPath "/old/parent1/c" & entryPathParentDir .~ unsafeMkPath "/new/parent2"
-- "/new/parent2/c"
entryPathParentDir :: Lens' EntryPath Path
entryPathParentDir = isoEntryPath . initNE . from isoPath

-- | Retrieve a list of a path's segments.
--
-- >>> entryPathSegments $ unsafeMkEntryPath "/a/b/c"
-- ["a","b","c"]
entryPathSegments :: EntryPath -> [Text]
entryPathSegments p = p ^.. isoEntryPath . each . to unPathSegment

-- | Build an `EntryPath` by append the entry's name to its location path.
--
-- >>> build $ appendEntryName (unsafeMkPath "/my/directory") (unsafeMkPathSegment "entryname")
-- "/my/directory/entryname"
appendEntryName :: Path -> PathSegment -> EntryPath
appendEntryName (Path segments) entryName = EntryPath $ appendNE segments entryName

-- | Attempts to cast a `Path` to an `EntryPath`.
pathAsEntryPath :: Path -> Either Text EntryPath
pathAsEntryPath (Path segments) =
  case NE.nonEmpty segments of
    Just ep -> Right $ EntryPath ep
    Nothing -> Left "Entry paths must not be empty."

-- | Casts an `EntryPath` to a `Path`.
entryPathAsPath :: EntryPath -> Path
entryPathAsPath (EntryPath segments) = Path $ NE.toList segments

-- | Replaces the prefix of a given path.
--
-- >>> oldPrefix = unsafeMkPath "sre/github"
-- >>> newPrefix = unsafeMkPath "sre/gitlab"
-- >>> build $ replacePathPrefix oldPrefix newPrefix (unsafeMkPath "/sre/github/serokell-bot")
-- "/sre/gitlab/serokell-bot"
--
-- Returns `Nothing` when the given path does _not_ have the expected prefix.
replacePathPrefix :: Path -> Path -> Path -> Maybe Path
replacePathPrefix (Path oldPrefix) (Path newPrefix) (Path fullpath) =
  fullpath
    & List.stripPrefix oldPrefix
    <&> mappend newPrefix
    <&> Path

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Append an item to the end of a list.
appendNE :: [a] -> a -> NonEmpty a
appendNE xs last =
  case xs of
    [] -> last :| []
    (x : xs) -> x :| xs <> [last]

-- | Focus every element except the last.
initNE :: Lens' (NonEmpty a) [a]
initNE = lens NE.init \ne newInit -> appendNE newInit (NE.last ne)
