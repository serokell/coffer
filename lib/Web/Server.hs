-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Server where

import Control.Monad.Catch (SomeException, try)
import Control.Monad.Except

import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Polysemy
import Polysemy.Error hiding (try)


import Backend
import Backend.Vault.Kv
import CLI.Types
import Entry
import Error
import Web.API

import Backend.Interpreter (runBackend)
import Coffer.Directory (Directory)
import Coffer.Path (EntryPath, Path, QualifiedPath)
import Data.Functor ((<&>))
import Data.Set qualified as Set
import Fmt (pretty)
import Servant.API
import Servant.Client
import Servant.Server

url :: BaseUrl
url = BaseUrl Http "127.0.0.1" 8200 ""

mount :: Text
mount = "secret"

runBackendIO' :: Sem '[BackendEffect, Error CofferError, Embed IO, Final IO] a -> IO (Either CofferError a)
runBackendIO' action =
  runBackend action
    & errorToIOFinal @CofferError
    & embedToFinal @IO
    & runFinal

reportErrors :: IO (Either CofferError a) -> Handler a
reportErrors io = do
  eea <- liftIO do
    try @_ @SomeException io

  case eea of
    Left ioError -> do
      throwError err500 { errBody = errorToServantErrorMsg ioError }

    Right (Left e) -> do
      throwError err500 { errBody = errorToServantErrorMsg (pretty @_ @Text e) }

    Right (Right a) -> do
      return a

errorToServantErrorMsg :: Show a => a -> Bytes.Lazy.ByteString
errorToServantErrorMsg
  = Bytes.Lazy.fromStrict
  . Text.encodeUtf8
  . Text.pack
  . show

makeServer
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> Server API
makeServer run token
  =    view   run token
  :<|> create run token
  :<|>
    (\txt fkey ->
         private run token txt fkey
    :<|> public  run token txt fkey
    :<|> set     run token txt fkey
    )
  :<|> deleteField run token
  :<|> find'       run token
  :<|> rename      run token
  :<|> copy'       run token
  :<|> delete'     run token
  :<|> tag         run token

view
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath Path
  -> Maybe FieldName
  -> Handler ViewResult
view run token voQPath voFieldName = do
  run token $ CmdView ViewOptions {voQPath, voFieldName}

create
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> Bool
  -> [EntryTag]
  -> (HashMap FieldName Text, HashMap FieldName Text)
  -> Handler CreateResult
create run token coQPath coForce coTags (fields, privateFields) =
  run token $ CmdCreate CreateOptions
    { coQPath
    , coEdit = False
    , coForce
    , coTags          = Set.fromList coTags
    , coFields        = fields        & HashMap.toList <&> do \(fieldName, fieldContents) -> FieldInfo fieldName (FieldContents fieldContents)
    , coPrivateFields = privateFields & HashMap.toList <&> do \(fieldName, fieldContents) -> FieldInfo fieldName (FieldContents fieldContents)
    }

private
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler SetFieldResult
private run token sfoQPath sfoFieldName  = do
  run token $ CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Private
    }

public
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler SetFieldResult
public run token sfoQPath sfoFieldName  =
  run token $ CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Public
    }

set
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Maybe FieldContents
  -> Handler SetFieldResult
set run token sfoQPath sfoFieldName sfoFieldContents =
  run token $ CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = sfoFieldContents
    , sfoVisibility = Nothing
    }

deleteField
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler DeleteFieldResult
deleteField run token dfoQPath dfoFieldName =
  run token $ CmdDeleteField DeleteFieldOptions
    { dfoQPath
    , dfoFieldName
    }

find'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Maybe (QualifiedPath Path)
  -> Maybe Text
  -> Maybe (Sort, Direction)
  -> [Filter]
  -> Handler (Maybe Directory)
find' run token foQPath foText foSort foFilters =
  run token $ CmdFind FindOptions
    { foQPath
    , foText
    , foSort
    , foFilters
    }

rename
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Bool
  -> QualifiedPath Path
  -> QualifiedPath Path
  -> Bool
  -> Handler RenameResult
rename run token roDryRun roQOldPath roQNewPath roForce =
  run token $ CmdRename RenameOptions
    { roDryRun
    , roQOldPath
    , roQNewPath
    , roForce
    }

copy'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Bool
  -> QualifiedPath Path
  -> QualifiedPath Path
  -> Bool
  -> Handler CopyResult
copy' run token cpoDryRun cpoQOldPath cpoQNewPath cpoForce =
  run token $ CmdCopy CopyOptions
    { cpoDryRun
    , cpoQNewPath
    , cpoQOldPath
    , cpoForce
    }

delete'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Bool
  -> QualifiedPath Path
  -> Bool
  -> Handler DeleteResult
delete' run token doDryRun doQPath doRecursive =
  run token $ CmdDelete DeleteOptions
    { doDryRun
    , doQPath
    , doRecursive
    }

tag
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> EntryTag
  -> Bool
  -> Handler TagResult
tag run token toQPath toTagName toDelete =
  run token $ CmdTag TagOptions
    { toQPath
    , toTagName
    , toDelete
    }
