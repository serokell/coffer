-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Coffer.Path
  ( PathSegment
  , unPathSegment
  , mkPathSegment
  , pathSegmentAllowedCharacters
  , DirectoryContents(..)
  , directoryNames
  , entryNames
  , HasPathSegments(..)
  , Path(..)
  , mkPath
  , EntryPath(..)
  , mkEntryPath
  , entryPathName
  , entryPathParentDir
  , entryPathParentDirs
  , appendEntryName
  , pathAsEntryPath
  , entryPathAsPath
  , replacePathPrefix
  , QualifiedPath (..)
  ) where

import BackendName (BackendName, newBackendName)
import Control.Lens
import Control.Monad ((>=>))
import Data.Aeson (ToJSON, Value(String))
import Data.Aeson qualified as A
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Fmt (Buildable, build, fmt, pretty)
import GHC.Generics (Generic)
import Servant (FromHttpApiData(..), ToHttpApiData(..))

-- $setup
-- >>> import Fmt (pretty, build)
-- >>> import Control.Lens
-- >>> import qualified Data.List.NonEmpty as NE
-- >>> isRight (Right a) = a
-- >>> unsafeMkPath = isRight . mkPath
-- >>> unsafeMkEntryPath = isRight . mkEntryPath
-- >>> unsafeMkPathSegment = isRight . mkPathSegment

newtype PathSegment = UnsafeMkPathSegment { unPathSegment :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Buildable, ToHttpApiData, FromHttpApiData)
  deriving newtype (Hashable, A.ToJSON, A.ToJSONKey)

data DirectoryContents = DirectoryContents
  { dcDirectoryNames :: [PathSegment]
  , dcEntryNames :: [PathSegment]
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''DirectoryContents

mkPathSegment :: Text -> Either Text PathSegment
mkPathSegment segment
  | T.null segment =
      Left "Path segments must contain at least 1 character"
  | T.any (`notElem` pathSegmentAllowedCharacters) segment =
      Left $ "Path segments can only contain the following characters: '" <> T.pack pathSegmentAllowedCharacters <> "'"
  | otherwise = Right $ UnsafeMkPathSegment segment

pathSegmentAllowedCharacters :: [Char]
pathSegmentAllowedCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'8'] ++ "-_"

-- | Path to a directory or an entry.
newtype Path = Path { unPath :: [PathSegment] }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving newtype (Hashable, A.ToJSON, A.ToJSONKey)

instance ToHttpApiData Path where
  toUrlPiece = fmt . build

instance FromHttpApiData Path where
  parseUrlPiece = mkPath

-- |
-- >>> pretty @Path @String <$> mkPath "a/b/c"
-- Right "/a/b/c"
instance Buildable Path where
  build (Path segments) = "/" <> build (T.intercalate "/" $ pretty <$> segments)

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance A.ToJSON EntryPath where
  toJSON = String . pretty

instance ToHttpApiData EntryPath where
  toUrlPiece = fmt . build

instance FromHttpApiData EntryPath where
  parseUrlPiece = mkEntryPath

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
entryPathName = view $ pathSegments . last1 . to unPathSegment

-- | Focus the path to the directory this entry is in.
--
-- >>> unsafeMkEntryPath "/parent/dir/c" ^. entryPathParentDir . to build
-- "/parent/dir"
--
-- >>> build $ unsafeMkEntryPath "/old/parent1/c" & entryPathParentDir .~ unsafeMkPath "/new/parent2"
-- "/new/parent2/c"
entryPathParentDir :: Lens' EntryPath Path
entryPathParentDir = pathSegments . initNE . from pathSegments

-- | Gets all parents paths of given entry path.
--
-- >>> NE.toList (entryPathParentDirs (unsafeMkEntryPath "/dir1/dir2/entry")) <&> build
-- ["/","/dir1","/dir1/dir2"]
entryPathParentDirs :: EntryPath -> NonEmpty Path
entryPathParentDirs entryPath =
  entryPath ^. pathSegments
    & NE.init
    & NE.inits
    <&> Path

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

data QualifiedPath path = QualifiedPath
  { qpBackendName :: Maybe BackendName
  , qpPath :: path
  }
  deriving stock (Show, Eq, Functor)

instance (Buildable path) => ToJSON (QualifiedPath path) where
  toJSON = String . pretty

instance (Buildable path) => Buildable (QualifiedPath path) where
  build (QualifiedPath backendNameMb path) =
    case backendNameMb of
      Just backendName -> build backendName <> "#" <> build path
      Nothing -> build path

instance (FromHttpApiData path) => FromHttpApiData (QualifiedPath path) where
  parseUrlPiece text
    | [pathPiece] <- T.splitOn "#" text = do
        path <- parseUrlPiece @path pathPiece
        pure $ QualifiedPath Nothing path
    | [backendPiece, pathPiece] <- T.splitOn "#" text = do
        backendName <- newBackendName backendPiece
        path <- parseUrlPiece @path pathPiece
        pure $ QualifiedPath (Just backendName) path
    | otherwise =
        Left "Invalid qualified path format. Expected [BACKENDNAME#]PATH"

instance (ToHttpApiData path, Buildable path) => ToHttpApiData (QualifiedPath path) where
  toUrlPiece = pretty

----------------------------------------------------------------------------
-- Optics
----------------------------------------------------------------------------

class HasPathSegments s pathSegments | s -> pathSegments where
  pathSegments :: Iso' s pathSegments
instance HasPathSegments Path [PathSegment] where
  pathSegments = iso unPath Path
instance HasPathSegments EntryPath (NonEmpty PathSegment) where
  pathSegments = iso unEntryPath EntryPath

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
