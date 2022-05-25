-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Server where

import Control.Monad.Catch (Exception(displayException), SomeException, try)
import Control.Monad.Except

import Data.Function ((&))
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)

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
import Coffer.PrettyPrint
  (PrettyPrintMode(WebAPI), buildCopyOrRenameResult, buildCreateError, buildDeleteFieldResult,
  buildDeleteResult, buildSetFieldResult, buildTagResult, buildViewResult)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Functor ((<&>))
import Data.Set qualified as Set
import Data.Text qualified as T
import Fmt (Builder, pretty, unlinesF)
import GHC.Generics (Generic)
import Servant.API
import Servant.Server
import Web.Types (NewEntry(NewEntry), NewField(NewField))

data CofferServerError = CofferServerError
  { cseError :: Text
  , cseCode :: Int
  }
  deriving stock (Show, Generic)
deriveJSON (aesonPrefix camelCase) ''CofferServerError

resultToText :: (PrettyPrintMode -> a -> Builder) -> a -> Text
resultToText f res = pretty $ f WebAPI res

throwCofferServerError :: ServerError -> Int -> Text -> Handler a
throwCofferServerError err code msg = throwCofferServerErrors err [(code, msg)]

throwCofferServerErrors :: ServerError -> [(Int, Text)] -> Handler a
throwCofferServerErrors err codesAndMsgs = do
  let cofferServerErrors = uncurry (flip CofferServerError) <$> codesAndMsgs
  throwError err
    { errBody = encode cofferServerErrors
    , errHeaders = ("Content-Type", "application/json;charset=utf-8") : errHeaders err
    }

handleSetFieldResult :: SetFieldResult -> Handler SetFieldResult
handleSetFieldResult = \case
  res@SFRSuccess{} -> pure res
  res@SFREntryNotFound{} ->
    throwCofferServerError err404 300 (pretty res)
  res@SFRMissingFieldContents{} ->
    throwCofferServerError err400 301 (pretty res)
  where
    pretty = resultToText buildSetFieldResult

handleCopyOrRenameResult :: Bool -> CopyResult -> Handler CopyResult
handleCopyOrRenameResult rename = \case
  res@CPRSuccess{} -> pure res
  res@CPRPathNotFound{} ->
    throwCofferServerError err404 500 (prettySingleMessage res)
  res@CPRMissingEntryName{} ->
    throwCofferServerError err400 501 (prettySingleMessage res)
  res@CPRSamePath{} ->
    throwCofferServerError err409 502 (prettySingleMessage res)
  res@(CPRCreateErrors createErrors) -> do
    let messages = buildCopyOrRenameResult rename WebAPI res <&> pretty
    let codes = createErrors <&> createErrorToCode . snd
    throwCofferServerErrors err409 (zip codes messages)
  where
    prettySingleMessage :: CopyResult -> Text
    prettySingleMessage res =
      buildCopyOrRenameResult rename WebAPI res
        & unlinesF
        & pretty

    createErrorToCode :: CreateError -> Int
    createErrorToCode = \case
      CEParentDirectoryIsEntry{} -> 503
      CEDestinationIsDirectory{} -> 504
      CEEntryAlreadyExists{} -> 505

handleCopyResult :: CopyResult -> Handler CopyResult
handleCopyResult = handleCopyOrRenameResult False

handleRenameResult :: RenameResult -> Handler RenameResult
handleRenameResult = handleCopyOrRenameResult True

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
      throwCofferServerError err500 0 (T.pack $ displayException ioError)

    Right (Left e) -> do
      throwCofferServerError err500 0 (pretty e)

    Right (Right a) -> do
      return a

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
  :<|>
    (\path tag' ->
         tag run token path tag' False
    :<|> tag run token path tag' True
    )

view
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath Path
  -> Maybe FieldName
  -> Handler ViewResult
view run token voQPath voFieldName = do
  run token (CmdView ViewOptions {voQPath, voFieldName}) >>= \case
    res@VRDirectory{} -> pure res
    res@VREntry{} -> pure res
    res@VRField{} -> pure res
    res@VRPathNotFound{} ->
      throwCofferServerError err404 100 (pretty res)
    res@VRDirectoryNoFieldMatch{} ->
      throwCofferServerError err404 101 (pretty res)
    res@VREntryNoFieldMatch{} ->
      throwCofferServerError err404 102 (pretty res)
  where
    pretty = resultToText buildViewResult

create
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> Bool
  -> NewEntry
  -> Handler CreateResult
create run token coQPath coForce (NewEntry coFields coTags) =
  run token (CmdCreate CreateOptions
    { coQPath
    , coEdit = False
    , coForce
    , coTags          = Set.fromList coTags
    , coFields        = fst <$> (filter ((==Public) . snd) allFields)
    , coPrivateFields = fst <$> (filter ((==Private) . snd) allFields)
    }) >>= \case
      res@CRSuccess{} -> pure res
      CRCreateError createError ->
        case createError of
          res@CEParentDirectoryIsEntry{} ->
            throwCofferServerError err409 200 (pretty res)
          res@CEDestinationIsDirectory{} ->
            throwCofferServerError err409 201 (pretty res)
          res@CEEntryAlreadyExists{} ->
            throwCofferServerError err409 202 (pretty res)
  where
    allFields :: [(FieldInfo, FieldVisibility)]
    allFields = coFields
      & HashMap.toList
      <&> \(name, NewField contents visibility) -> (FieldInfo name contents, visibility)

    pretty = resultToText buildCreateError

private
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler SetFieldResult
private run token sfoQPath sfoFieldName  = do
  run token (CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Private
    }) >>= handleSetFieldResult

public
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler SetFieldResult
public run token sfoQPath sfoFieldName  =
  run token (CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Public
    }) >>= handleSetFieldResult

set
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Maybe FieldContents
  -> Handler SetFieldResult
set run token sfoQPath sfoFieldName sfoFieldContents =
  run token (CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = sfoFieldContents
    , sfoVisibility = Nothing
    }) >>= handleSetFieldResult

deleteField
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler DeleteFieldResult
deleteField run token dfoQPath dfoFieldName =
  run token (CmdDeleteField DeleteFieldOptions
    { dfoQPath
    , dfoFieldName
    }) >>= \case
      res@DFRSuccess{} -> pure res
      res@DFREntryNotFound{} ->
        throwCofferServerError err404 400 (pretty res)
      res@DFRFieldNotFound{} ->
        throwCofferServerError err404 401 (pretty res)
  where
    pretty = resultToText buildDeleteFieldResult

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
  run token (CmdRename RenameOptions
    { roDryRun
    , roQOldPath
    , roQNewPath
    , roForce
    }) >>= handleRenameResult

copy'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Bool
  -> QualifiedPath Path
  -> QualifiedPath Path
  -> Bool
  -> Handler CopyResult
copy' run token cpoDryRun cpoQOldPath cpoQNewPath cpoForce =
  run token (CmdCopy CopyOptions
    { cpoDryRun
    , cpoQNewPath
    , cpoQOldPath
    , cpoForce
    }) >>= handleCopyResult

delete'
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> Bool
  -> QualifiedPath Path
  -> Bool
  -> Handler DeleteResult
delete' run token doDryRun doQPath doRecursive =
  run token (CmdDelete DeleteOptions
    { doDryRun
    , doQPath
    , doRecursive
    }) >>= \case
      res@DRSuccess{} -> pure res
      res@DRPathNotFound{} ->
        throwCofferServerError err404 600 (pretty res)
      res@DRDirectoryFound{} ->
        throwCofferServerError err400 601 (pretty res)
  where
    pretty = resultToText buildDeleteResult

tag
  :: (forall a. VaultToken -> Command a -> Handler a)
  -> VaultToken
  -> QualifiedPath EntryPath
  -> EntryTag
  -> Bool
  -> Handler TagResult
tag run token toQPath toTagName toDelete =
  run token (CmdTag TagOptions
    { toQPath
    , toTagName
    , toDelete
    }) >>= \case
      res@TRSuccess{} -> pure res
      res@TREntryNotFound{} ->
        throwCofferServerError err404 700 (pretty res)
      res@TRTagNotFound{} ->
        throwCofferServerError err404 701 (pretty res)
      res@TRDuplicateTag{} ->
        throwCofferServerError err409 702 (pretty res)
  where
    pretty = resultToText buildTagResult
