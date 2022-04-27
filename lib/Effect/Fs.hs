-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Effect.Fs
  ( FsEffect
  , nodeExists
  , getNode
  , listDirectory
  , listDirectoryRec
  , runFsInIO
  , stringToPath
  , pathToString
  , FsError
  , Node
  , Node'
  , File (..)
  , Directory (..)
  )
  where

import Control.Lens
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Polysemy
import Polysemy.Error
import System.Directory hiding (listDirectory)
import System.Directory qualified as D
import Data.Text (Text)

type Node f d = Either (File f) (Directory d)
type Node' a = Node a a
type Path = ByteString

newtype File a = File a
  deriving stock (Show)
newtype Directory a = Directory a
  deriving stock (Show)

data FsError
  = FENodeNotFound Path
  | FENodeExists (MismatchError (Node' Path))
  | FEMissingParent (Node' Path)
  | FEInvalidPath Text Path
  deriving stock (Show)

data MismatchError a = MismatchError
  { meWanted :: a
  , mwFound :: a
  }
  deriving stock (Show)

newtype NodeRec = NodeRec (Node Path [NodeRec])

pathToString :: Path -> Either FsError String
pathToString path =
  case decodeUtf8' path of
    Left a -> Left $ FEInvalidPath (T.pack $ show a) path
    Right b -> Right $ T.unpack b
    
stringToPath :: String -> Path
stringToPath = encodeUtf8 . T.pack

eitherError
  :: Member (Error e) r
  => (a -> e)
  -> Either a b
  -> Sem r b
eitherError f = either (throw . f) pure

data FsEffect m a where
  -- | Checks whether a 'Node' exists and retuns 'Just (Node' ())' or 'Nothing'
  -- | if the node wasn't found. 'Node'' is either a 'Directory ()' or a
  -- | 'File ()' according to what was found.
  NodeExists :: Path -> FsEffect m (Maybe (Node' ()))
  GetNode :: Path -> FsEffect m (Maybe (Node' ByteString))
  -- | Lists a directories contents, returns the path to each of its contents.
  -- | If the directory is not found or it's a file, it returns a 'Nothing'
  ListDirectory :: Directory Path -> FsEffect m (Maybe [Node' Path])
  ListDirectoryRec :: Directory Path -> FsEffect m [NodeRec]
--  ReadNode :: Node' Path -> FsEffect m (Node ByteString [ByteString])
--  CreateNode :: Node' Path -> FsEffect m (Node' ())
--  GetHandle :: File Path -> FsEffect m (File Handle)
makeSem ''FsEffect

runFsInIO
  :: Member (Error FsError) r
  => Member (Embed IO) r
  => Sem (FsEffect ': r) a
  -> Sem r a
runFsInIO = interpret
  \case
    NodeExists path -> _nodeExists path
    GetNode path -> _getNode path
    ListDirectory dirPath -> _listDirectory dirPath
    ListDirectoryRec dirPath -> _listDirectoryRec dirPath
    -- ReadNode nodePath -> undefined
    -- CreateNode nodePath -> undefined

_nodeExists
  :: ( Member (Error FsError) r
     , Member (Embed IO) r
     )
  => Path
  -> Sem r (Maybe (Node' ()))
_nodeExists path = do
  stringPath <-
    eitherError
    (\unicodeError -> FEInvalidPath (T.pack $ show unicodeError) path)
    (decodeUtf8' path <&> T.unpack)
  file <- embed $ doesFileExist stringPath
  dir <- embed $ doesDirectoryExist stringPath

  case (file, dir) of
    (True, False) -> pure . Just . Left $ File ()
    (False, True) -> pure . Just . Right $ Directory ()
    (_, _) -> pure Nothing

_getNode
  :: ( Member (Error FsError) r
     , Member (Embed IO) r
     )
  => Path
  -> Sem r (Maybe (Node' Path))
_getNode path = do
  mNode <- _nodeExists path
  pure
    $ mNode <&> bimap
        (const (File path))
        (const (Directory path))


_listDirectory
  :: forall r.
     ( Member (Error FsError) r
     , Member (Embed IO) r
     )
  => Directory Path
  -> Sem r (Maybe [Node' Path])
_listDirectory (Directory path) = do
  stringPath <- eitherError id (pathToString path)
  nodes <- embed $ D.listDirectory stringPath

  _getNode path >>=
    \case
      Just _ -> mapM (_getNodeThrow . stringToPath) nodes <&> Just
      _ -> pure Nothing

  where
    _getNodeThrow
      :: Path
      -> Sem r (Node' Path)
    _getNodeThrow path =
      _getNode path >>= maybe (throw $ FENodeNotFound path) pure

_listDirectoryRec
  :: ( Member (Error FsError) r
     , Member (Embed IO) r
     )
  => Directory Path
  -> Sem r [NodeRec]
_listDirectoryRec dirPath@(Directory path) =
  _listDirectory dirPath >>= \case
    Nothing -> throw $ FENodeNotFound path
    Just nodes -> do
      forM nodes $ \case
        Left f -> pure $ NodeRec $ Left f
        Right d ->
          _listDirectoryRec d <&> NodeRec . Right . Directory

-- _readNode
--   :: Member (Error FsError) r
--   => Member (Embed IO) r
--   => Node' Path
--   -> Sem r (Node ByteString [ByteString])
-- _readNode nodePath = undefined

-- _createNode
--   :: Member (Error FsError) r
--   => Member (Embed IO) r
--   => Node' Path
--   -> Sem r ()
-- _createNode nodePath = undefined
