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
import Coffer.Path (EntryPath, Path, QualifiedPath(QualifiedPath, qpPath))
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
import Polysemy.Async (Async, asyncToIOFinal)
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

runBackendIO' :: Sem '[BackendEffect, Error CofferError, Embed IO, Async, Final IO] a -> IO (Either CofferError a)
runBackendIO' action =
  runBackend action
    & errorToIOFinal @CofferError
    & embedToFinal @IO
    & asyncToIOFinal
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
  -> Path
  -> Handler Directory
view run voPath = do
  run ( CmdView ViewOptions
    { voQPath = QualifiedPath Nothing voPath
    , voFieldName = Nothing
    }) >>= \case
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
  -> EntryPath
  -> Bool
  -> NewEntry
  -> Handler Entry
create run coPath coForce (NewEntry coFields coTags) =
  run (CmdCreate CreateOptions
    { coQPath = QualifiedPath Nothing coPath
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
  -> EntryPath
  -> FieldName
  -> Handler Entry
private run sfoPath sfoFieldName  = do
  run (CmdSetField SetFieldOptions
    { sfoQPath = QualifiedPath Nothing sfoPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Private
    }) >>= handleSetFieldResult

public
  :: (forall a. Command a -> Handler a)
  -> EntryPath
  -> FieldName
  -> Handler Entry
public run sfoPath sfoFieldName  =
  run (CmdSetField SetFieldOptions
    { sfoQPath = QualifiedPath Nothing sfoPath
    , sfoFieldName
    , sfoFieldContents = Nothing
    , sfoVisibility = Just Public
    }) >>= handleSetFieldResult

set
  :: (forall a. Command a -> Handler a)
  -> EntryPath
  -> FieldName
  -> Maybe FieldContents
  -> Handler Entry
set run sfoPath sfoFieldName sfoFieldContents =
  run (CmdSetField SetFieldOptions
    { sfoQPath = QualifiedPath Nothing sfoPath
    , sfoFieldName
    , sfoFieldContents = sfoFieldContents
    , sfoVisibility = Nothing
    }) >>= handleSetFieldResult

deleteField
  :: (forall a. Command a -> Handler a)
  -> EntryPath
  -> FieldName
  -> Handler Entry
deleteField run dfoPath dfoFieldName =
  run (CmdDeleteField DeleteFieldOptions
    { dfoQPath = QualifiedPath Nothing dfoPath
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
  -> Maybe Path
  -> Maybe Text
  -> Maybe (Sort, Direction)
  -> [Filter]
  -> Handler (Maybe Directory)
find' run foPath foText foSort foFilters =
  run $ CmdFind FindOptions
    { foQPath = (fmap (QualifiedPath Nothing) foPath)
    , foText
    , foSort
    , foFilters
    }

rename
  :: (forall a. Command a -> Handler a)
  -> Bool
  -> Path
  -> Path
  -> Bool
  -> Handler [(EntryPath, EntryPath)]
rename run roDryRun roOldPath roNewPath roForce =
  run (CmdRename RenameOptions
    { roDryRun
    , roQOldPath = QualifiedPath Nothing roOldPath
    , roQNewPath = QualifiedPath Nothing roNewPath
    , roForce
    }) >>= handleRenameResult

copy'
  :: (forall a. Command a -> Handler a)
  -> Bool
  -> Path
  -> Path
  -> Bool
  -> Handler [(EntryPath, EntryPath)]
copy' run cpoDryRun cpoOldPath cpoNewPath cpoForce =
  run (CmdCopy CopyOptions
    { cpoDryRun
    , cpoQNewPath = QualifiedPath Nothing cpoNewPath
    , cpoQOldPath = QualifiedPath Nothing cpoOldPath
    , cpoForce
    }) >>= handleCopyResult

delete'
  :: (forall a. Command a -> Handler a)
  -> Bool
  -> Path
  -> Bool
  -> Handler NoContent
delete' run doDryRun doPath doRecursive =
  run (CmdDelete DeleteOptions
    { doDryRun
    , doQPath = QualifiedPath Nothing doPath
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
  -> EntryPath
  -> EntryTag
  -> Bool
  -> Handler Entry
tag run toPath toTagName toDelete =
  run (CmdTag TagOptions
    { toQPath = QualifiedPath Nothing toPath
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
