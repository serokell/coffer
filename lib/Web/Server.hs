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

import BackendEffect (BackendEffect)
import Backends (SomeBackend)
import CLI.Types
import Entry
import Error
import Web.API

import Backend.Interpreter (runBackend)
import Coffer.Directory (Directory, singleton)
import Coffer.Path (EntryPath, Path, QualifiedPath(qpPath))
import Coffer.PrettyPrint
  (PrettyPrintMode(WebAPI), buildCopyOrRenameResult, buildCreateError, buildDeleteFieldResult,
  buildDeleteResult, buildSetFieldResult, buildTagResult, buildViewResult)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Bifunctor (bimap)
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

handleSetFieldResult :: SetFieldResult -> Handler Entry
handleSetFieldResult = \case
  SFRSuccess _ qEntry -> pure $ qpPath qEntry
  res@SFREntryNotFound{} ->
    throwCofferServerError err404 300 (pretty res)
  res@SFRMissingFieldContents{} ->
    throwCofferServerError err400 301 (pretty res)
  where
    pretty = resultToText buildSetFieldResult

handleCopyOrRenameResult :: Bool -> CopyResult -> Handler [(EntryPath, EntryPath)]
handleCopyOrRenameResult rename = \case
  CPRSuccess _ paths -> pure (paths <&> bimap qpPath qpPath)
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

handleCopyResult :: CopyResult -> Handler [(EntryPath, EntryPath)]
handleCopyResult = handleCopyOrRenameResult False

handleRenameResult :: RenameResult -> Handler [(EntryPath, EntryPath)]
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
  :: (SomeBackend -> (forall a. Command a -> Handler a))
  -> Server API
makeServer run backend
  =    view   (run backend)
  :<|> create (run backend)
  :<|>
    (\txt fkey ->
         private (run backend) txt fkey
    :<|> public  (run backend) txt fkey
    :<|> set     (run backend) txt fkey
    )
  :<|> deleteField (run backend)
  :<|> find'       (run backend)
  :<|> rename      (run backend)
  :<|> copy'       (run backend)
  :<|> delete'     (run backend)
  :<|>
    (\path tag' ->
         tag (run backend) path tag' False
    :<|> tag (run backend) path tag' True
    )

view
  :: (forall a. Command a -> Handler a)
  -> QualifiedPath Path
  -> Handler Directory
view run voQPath = do
  run (CmdView ViewOptions {voQPath, voFieldName = Nothing}) >>= \case
    VRDirectory dir -> pure dir
    VREntry entry -> pure $ singleton entry
    VRField{} ->
      throwCofferServerError err500 0 "Unexpected VRField"
    res@VRPathNotFound{} ->
      throwCofferServerError err404 100 (pretty res)
    res@VRDirectoryNoFieldMatch{} ->
      throwCofferServerError err404 101 (pretty res)
    res@VREntryNoFieldMatch{} ->
      throwCofferServerError err404 102 (pretty res)
  where
    pretty = resultToText buildViewResult

create
  :: (forall a. Command a -> Handler a)
  -> QualifiedPath EntryPath
  -> Bool
  -> NewEntry
  -> Handler Entry
create run coQPath coForce (NewEntry coFields coTags) =
  run (CmdCreate CreateOptions
    { coQPath
    , coEdit = False
    , coForce
    , coTags          = Set.fromList coTags
    , coFields        = fst <$> (filter ((==Public) . snd) allFields)
    , coPrivateFields = fst <$> (filter ((==Private) . snd) allFields)
    }) >>= \case
      CRSuccess qEntry -> pure $ qpPath qEntry
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
  :: (forall a. Command a -> Handler a)
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler Entry
private run sfoQPath sfoFieldName  = do
  run (CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Private
    }) >>= handleSetFieldResult

public
  :: (forall a. Command a -> Handler a)
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler Entry
public run sfoQPath sfoFieldName  =
  run (CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Public
    }) >>= handleSetFieldResult

set
  :: (forall a. Command a -> Handler a)
  -> QualifiedPath EntryPath
  -> FieldName
  -> Maybe FieldContents
  -> Handler Entry
set run sfoQPath sfoFieldName sfoFieldContents =
  run (CmdSetField SetFieldOptions
    { sfoQPath
    , sfoFieldName
    , sfoFieldContents = sfoFieldContents
    , sfoVisibility = Nothing
    }) >>= handleSetFieldResult

deleteField
  :: (forall a. Command a -> Handler a)
  -> QualifiedPath EntryPath
  -> FieldName
  -> Handler Entry
deleteField run dfoQPath dfoFieldName =
  run (CmdDeleteField DeleteFieldOptions
    { dfoQPath
    , dfoFieldName
    }) >>= \case
      DFRSuccess _ qEntry -> pure $ qpPath qEntry
      res@DFREntryNotFound{} ->
        throwCofferServerError err404 400 (pretty res)
      res@DFRFieldNotFound{} ->
        throwCofferServerError err404 401 (pretty res)
  where
    pretty = resultToText buildDeleteFieldResult

find'
  :: (forall a. Command a -> Handler a)
  -> Maybe (QualifiedPath Path)
  -> Maybe Text
  -> Maybe (Sort, Direction)
  -> [Filter]
  -> Handler (Maybe Directory)
find' run foQPath foText foSort foFilters =
  run $ CmdFind FindOptions
    { foQPath
    , foText
    , foSort
    , foFilters
    }

rename
  :: (forall a. Command a -> Handler a)
  -> Bool
  -> QualifiedPath Path
  -> QualifiedPath Path
  -> Bool
  -> Handler [(EntryPath, EntryPath)]
rename run roDryRun roQOldPath roQNewPath roForce =
  run (CmdRename RenameOptions
    { roDryRun
    , roQOldPath
    , roQNewPath
    , roForce
    }) >>= handleRenameResult

copy'
  :: (forall a. Command a -> Handler a)
  -> Bool
  -> QualifiedPath Path
  -> QualifiedPath Path
  -> Bool
  -> Handler [(EntryPath, EntryPath)]
copy' run cpoDryRun cpoQOldPath cpoQNewPath cpoForce =
  run (CmdCopy CopyOptions
    { cpoDryRun
    , cpoQNewPath
    , cpoQOldPath
    , cpoForce
    }) >>= handleCopyResult

delete'
  :: (forall a. Command a -> Handler a)
  -> Bool
  -> QualifiedPath Path
  -> Bool
  -> Handler NoContent
delete' run doDryRun doQPath doRecursive =
  run (CmdDelete DeleteOptions
    { doDryRun
    , doQPath
    , doRecursive
    }) >>= \case
      DRSuccess{} -> pure NoContent
      res@DRPathNotFound{} ->
        throwCofferServerError err404 600 (pretty res)
      res@DRDirectoryFound{} ->
        throwCofferServerError err400 601 (pretty res)
  where
    pretty = resultToText buildDeleteResult

tag
  :: (forall a. Command a -> Handler a)
  -> QualifiedPath EntryPath
  -> EntryTag
  -> Bool
  -> Handler Entry
tag run toQPath toTagName toDelete =
  run (CmdTag TagOptions
    { toQPath
    , toTagName
    , toDelete
    }) >>= \case
      TRSuccess qEntry _ _ -> pure $ qpPath qEntry
      res@TREntryNotFound{} ->
        throwCofferServerError err404 700 (pretty res)
      res@TRTagNotFound{} ->
        throwCofferServerError err404 701 (pretty res)
      res@TRDuplicateTag{} ->
        throwCofferServerError err409 702 (pretty res)
  where
    pretty = resultToText buildTagResult
