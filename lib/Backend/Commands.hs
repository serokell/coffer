-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Commands where

import BackendEffect (BackendEffect, deleteEntry, listDirectoryContents, readEntry, writeEntry)
import BackendName (BackendName)
import Backends (SomeBackend)
import CLI.Types
import Coffer.Directory (Directory)
import Coffer.Directory qualified as Dir
import Coffer.Path
  (DirectoryContents(DirectoryContents), EntryPath, Path(Path),
  QualifiedPath(QualifiedPath, qpBackendName, qpPath), directoryNames, entryNames, entryPathName,
  pathSegments, unPathSegment)
import Coffer.Path qualified as Path
import Coffer.Util (catchAndReturn)
import Config (Config(backends, mainBackend))
import Control.Lens (view)
import Control.Lens hiding (view)
import Control.Monad.Extra (whenM)
import Control.Monad.State
import Data.Bifunctor (Bifunctor(first))
import Data.Either (rights)
import Data.HashMap.Strict ((!?))
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.Calendar.Month.Compat (pattern MonthDay)
import Entry
  (Entry, EntryTag, Field, FieldName, FieldVisibility(..), contents, dateModified, fieldContents,
  fields, newEntry, newField, path, visibility)
import Entry qualified as E
import Error (CofferError(..), InternalCommandsError(EntryPathDoesntHavePrefix))
import GHC.Exts (Down(..), sortWith)
import Polysemy
import Polysemy.Async (Async, sequenceConcurrently)
import Polysemy.Error (Error, throw)
import Validation (Validation(Failure, Success))

runCommand
  :: (Members '[BackendEffect, Embed IO, Error CofferError, Async] r)
  => Config -> Command res -> Sem r res
runCommand config = \case
  CmdView opts -> catchAndReturn $ viewCmd config opts
  CmdCreate opts -> catchAndReturn $ createCmd config opts
  CmdSetField opts -> catchAndReturn $ setFieldCmd config opts
  CmdDeleteField opts -> deleteFieldCmd config opts
  CmdFind opts -> findCmd config opts
  CmdRename opts -> catchAndReturn $ renameCmd config opts
  CmdCopy opts -> catchAndReturn $ copyCmd config opts
  CmdDelete opts -> catchAndReturn $ deleteCmd config opts
  CmdTag opts -> catchAndReturn $ tagCmd config opts

viewCmd
  :: (Members '[BackendEffect, Error CofferError, Error ViewResult] r)
  => Config -> ViewOptions -> Sem r ViewResult
viewCmd config (ViewOptions qPath@(QualifiedPath backendNameMb _) fieldNameMb) = do
  backend <- getBackend config backendNameMb
  getEntryOrDirThrow backend VRPathNotFound qPath >>= \case
    Right dir -> do
      case fieldNameMb of
        Nothing -> pure $ VRDirectory dir
        Just fieldName -> do
          pure $ dir
            -- If an entry has a field with the given fieldname, delete all non-matching fields.
            -- If it doesn't, delete the entire entry.
            & Dir.mapMaybe (\entry -> do
                field <- entry ^? fields . ix fieldName
                Just $ entry & fields .~ HashMap.singleton fieldName field
              )
            -- Delete empty dirs.
            & Dir.trimEmptyDirs
            & maybe (VRDirectoryNoFieldMatch qPath fieldName) VRDirectory
    Left entry ->
      case fieldNameMb of
        Nothing -> pure $ VREntry entry
        Just fieldName ->
          case entry ^? fields . ix fieldName of
            Just field -> pure $ VRField fieldName field
            Nothing -> do
              let qEntryPath = QualifiedPath backendNameMb (entry ^. E.path)
              pure $ VREntryNoFieldMatch qEntryPath fieldName

createCmd
  :: forall r
   . (Members '[BackendEffect, Embed IO, Error CofferError, Error CreateResult] r)
  => Config -> CreateOptions -> Sem r CreateResult
createCmd
  config
  (CreateOptions qEntryPath@(QualifiedPath backendNameMb entryPath) _edit force tags fields privateFields)
    = do
  backend <- getBackend config backendNameMb
  nowUtc <- embed getCurrentTime
  let
    mkField :: FieldVisibility -> FieldInfo -> (FieldName, Field)
    mkField fv fi = (fiName fi, newField nowUtc (fiContents fi) & visibility .~ fv)

    allFields = (mkField Public <$> fields) <> (mkField Private <$> privateFields)

    entry = newEntry entryPath nowUtc
      & E.fields .~ HashMap.fromList allFields
      & E.tags .~ tags

  checkCreateEntry backend backendNameMb force entry >>= \case
    Failure error -> throw $ CRCreateError error
    Success entry -> do
      void $ writeEntry backend entry
      pure $ CRSuccess qEntryPath { qpPath = entry }

setFieldCmd
  :: forall r
   . (Members '[BackendEffect, Embed IO, Error CofferError, Error SetFieldResult] r)
  => Config -> SetFieldOptions -> Sem r SetFieldResult
setFieldCmd
  config
  (SetFieldOptions qEntryPath@(QualifiedPath backendNameMb entryPath) fieldName fieldContentsMb visibilityMb)
    = do
  backend <- getBackend config backendNameMb
  readEntry backend entryPath >>= \case
    Nothing -> do
      pure $ SFREntryNotFound qEntryPath
    Just entry -> do
      nowUtc <- embed getCurrentTime
      updatedEntry <- updateOrInsert nowUtc entry
      void $ writeEntry backend updatedEntry
      pure $ SFRSuccess fieldName (QualifiedPath backendNameMb updatedEntry)
  where
    updateOrInsert :: UTCTime -> Entry -> Sem r Entry
    updateOrInsert nowUtc entry =
      entry
        & dateModified .~ nowUtc
        & fields . at fieldName %%~ updateOrInsertField nowUtc

    updateOrInsertField :: UTCTime -> Maybe Field -> Sem r (Maybe Field)
    updateOrInsertField nowUtc = \case
      Just field ->
        -- The field already exists, update it.
        pure $ Just $ field
          & dateModified .~ nowUtc
          & contents %~ do \currentContents -> fromMaybe currentContents fieldContentsMb
          & visibility %~ do \currentPrivate -> fromMaybe currentPrivate visibilityMb
      Nothing ->
        -- The field does not yet exist, insert a new one.
        case fieldContentsMb of
          Just fieldContents -> pure $ Just $ newField nowUtc fieldContents
            & visibility %~ do \currentPrivate -> fromMaybe currentPrivate visibilityMb
          -- If we're trying to insert a new field, but the user has not specified
          -- what the field contents should be, return an error.
          Nothing -> do
            let qEntryPath = QualifiedPath backendNameMb entryPath
            throw $ SFRMissingFieldContents fieldName qEntryPath

deleteFieldCmd
  :: (Members '[BackendEffect, Embed IO, Error CofferError] r)
  => Config -> DeleteFieldOptions -> Sem r DeleteFieldResult
deleteFieldCmd config (DeleteFieldOptions qPath@(QualifiedPath backendNameMb path) fieldName) = do
  backend <- getBackend config backendNameMb
  readEntry backend path >>= \case
    Nothing -> pure $ DFREntryNotFound qPath
    Just entry -> do
      case entry ^. fields . at fieldName of
        Nothing -> pure $ DFRFieldNotFound fieldName
        _ -> do
          nowUtc <- embed getCurrentTime
          let newEntry = entry
                & fields . at fieldName .~ Nothing
                & dateModified .~ nowUtc
          void $ writeEntry backend newEntry
          pure $ DFRSuccess fieldName qPath { qpPath = newEntry }

findCmd
  :: (Members '[BackendEffect, Error CofferError] r)
  => Config -> FindOptions -> Sem r (Maybe Directory)
findCmd config (FindOptions qPathMb textMb sortMb filters) = do
  let backendNameMb = qPathMb >>= qpBackendName
  backend <- getBackend config backendNameMb
  let
    filterByPath :: Entry -> Bool
    filterByPath e =
      case textMb of
        Nothing -> True
        Just text -> any (T.isInfixOf text) (e ^.. E.path . pathSegments . each . to unPathSegment)

    filterByTag :: Entry -> Bool
    filterByTag e =
      case textMb of
        Nothing -> True
        Just text -> any (T.isInfixOf text) (e ^.. E.tags . to Set.toList . each . to E.getEntryTag)

    applyFilter :: Entry -> Filter -> Bool
    applyFilter e = \case
      FilterByName substr -> substr `T.isInfixOf` (e ^. E.path . to entryPathName)
      FilterByDate op date -> matchDate op date (e ^. dateModified)
      FilterByField name field -> applyFilterField e name field

    applyFilterField :: Entry -> FieldName -> FilterField -> Bool
    applyFilterField e fieldName filter =
      case e ^? fields . ix fieldName of
        Nothing -> False
        Just field ->
          case filter of
            FilterFieldByContents substr -> substr `T.isInfixOf` (field ^. contents . fieldContents)
            FilterFieldByDate op date -> matchDate op date (field ^. dateModified)

  let path = maybe mempty qpPath qPathMb
  dir <- getEntryOrDir backend path <&> \case
    Just (Right dir) -> dir
    Just (Left entry) -> Dir.singleton entry
    Nothing -> Dir.emptyDir

  pure $
    dir
      & Dir.filterEntries (\e ->
          (filterByPath e || filterByTag e)
          && all (applyFilter e) filters
        )
      & Dir.mapDir (\entries ->
          case sortMb of
            Nothing -> entries
            Just (SortByEntryName, direction) ->
              sortWith' direction (view $ E.path . to entryPathName) entries
            Just (SortByFieldContents fieldName, direction) -> do
              let getFieldContents e = e ^? fields . ix fieldName . contents
              sortWith' direction getFieldContents entries
            Just (SortByEntryDate, direction) ->
              sortWith' direction (view E.dateModified) entries
            Just (SortByFieldDate fieldName, direction) -> do
              let getFieldDateModified e = e ^? fields . ix fieldName . dateModified
              sortWith' direction getFieldDateModified entries
        )
      & Dir.mapEntry hidePrivateFields
      & Dir.trimEmptyDirs

  where
    sortWith' :: forall a b. Ord b => Direction -> (a -> b) -> [a] -> [a]
    sortWith' Asc f = sortWith f
    sortWith' Desc f = sortWith (Down . f)

    hidePrivateFields :: Entry -> Entry
    hidePrivateFields e = e
      & fields
      . each
      . filtered (\field -> field ^. visibility == Private)
      . contents
      . fieldContents
      .~ "[private]"

    matchDate :: FilterOp -> FilterDate -> UTCTime -> Bool
    matchDate op filterDate date =
      case (filterDate, date) of
        (FDYear fyear, utctDay -> YearMonthDay year _ _) -> applyFilterOp op year fyear
        (FDMonth fmonth, utctDay -> MonthDay month _) -> applyFilterOp op month fmonth
        (FDDay fday, utctDay -> day) -> applyFilterOp op day fday
        (FDTime ftime, time) -> applyFilterOp op time ftime

    applyFilterOp :: Ord a => FilterOp -> a -> a -> Bool
    applyFilterOp = \case
      OpGT -> (>)
      OpGTE -> (>=)
      OpLT -> (<)
      OpLTE -> (<=)
      OpEQ -> (==)

renameCmd
  :: forall r
   . (Members '[BackendEffect, Embed IO, Error CofferError, Error RenameResult, Async] r)
  => Config -> RenameOptions -> Sem r RenameResult
renameCmd
  config
  (RenameOptions
    dryRun
    oldQPath@(QualifiedPath oldBackendNameMb _)
    newQPath@(QualifiedPath newBackendNameMb _)
    force
  )
    = do
  oldBackend <- getBackend config oldBackendNameMb
  newBackend <- getBackend config newBackendNameMb
  operations <- buildCopyOperations oldBackend newBackend oldQPath newQPath force

  unless dryRun do
    runCopyOperations newBackend operations

  -- we don't want to delete clashed entries if renaming is forced.
  let pathsToDelete =
        flip filter operations \(CopyOperation old _) ->
          none (\(CopyOperation _ new) -> qpPath old ^. path == qpPath new ^. path) operations

  -- If directory/entry was successfully copied,
  -- then we can delete old directory/entry without delete errors.
  unless dryRun do
    forM_ pathsToDelete \(CopyOperation old _) -> do
      deleteEntry oldBackend (qpPath old ^. path)

  pure $ CPRSuccess dryRun $ getOperationPaths <$> operations

data CopyOperation = CopyOperation
  { coQOld :: QualifiedPath Entry
  , coQNew :: QualifiedPath Entry
  }
  deriving stock Show

getOperationPaths :: CopyOperation -> (QualifiedPath EntryPath, QualifiedPath EntryPath)
getOperationPaths (CopyOperation old new) =
  (view E.path <$> old, view E.path <$> new)

{-# ANN buildCopyOperations ("HLint: ignore Redundant <$>" :: Text) #-}
buildCopyOperations
  :: forall r
   . (Members '[BackendEffect, Embed IO, Error CofferError, Error CopyResult] r)
  => SomeBackend -> SomeBackend -> QualifiedPath Path -> QualifiedPath Path -> Bool -> Sem r [CopyOperation]
buildCopyOperations
  oldBackend
  newBackend
  oldQPath@(QualifiedPath oldBackendNameMb oldPath)
  newQPath@(QualifiedPath newBackendNameMb newPath)
  force
    = do
  entryOrDir <- getEntryOrDirThrow oldBackend CPRPathNotFound oldQPath

  -- Don't need to copy directory or entry to itself
  when (oldQPath == newQPath) $ throw (CPRSamePath oldQPath)

  -- Build a list of operations to perform.
  nowUtc <- embed getCurrentTime
  operations <- fmap (setModifiedDate nowUtc) <$>
    case entryOrDir of
      Left entry -> copyEntry entry
      Right dir -> copyDir dir

  sequenceA <$> forM operations (validateCopyOperation newBackend) >>= \case
    Failure createErrors -> throw $ CPRCreateErrors createErrors
    Success _ -> pure operations

  where
    copyEntry :: Entry -> Sem r [CopyOperation]
    copyEntry entry =
      -- If the `oldPath` points to an entry,
      -- then `newPath` must also be a valid entry path.
      case Path.pathAsEntryPath newPath of
        Left _ -> throw CPRMissingEntryName
        Right newEntryPath -> do
          let old = QualifiedPath oldBackendNameMb entry
          let new = QualifiedPath newBackendNameMb (entry & E.path .~ newEntryPath)
          pure [CopyOperation old new]

    copyDir :: Directory -> Sem r [CopyOperation]
    copyDir dir = do
      forM (Dir.allEntries dir) \entry -> do
        -- Get the entry's parent dir, remove the `oldPath` prefix from it,
        -- and add the `newPath` prefix.
        newEntry <- entry & E.path . Path.entryPathParentDir %%~ \parentDir ->
          case Path.replacePathPrefix oldPath newPath parentDir of
            Just newParentDir -> pure newParentDir
            Nothing -> do
              let err = EntryPathDoesntHavePrefix (entry ^. E.path) oldPath
              throw $ InternalCommandsError err

        let old = QualifiedPath oldBackendNameMb entry
        let new = QualifiedPath newBackendNameMb newEntry

        pure $ CopyOperation old new

    setModifiedDate :: UTCTime -> CopyOperation -> CopyOperation
    setModifiedDate nowUtc (CopyOperation old new) =
      CopyOperation old (new <&> dateModified .~ nowUtc)

    -- | Performs a check on `CopyOperation` and returns @Failure@ if any of checks fail.
    validateCopyOperation
      :: SomeBackend
      -> CopyOperation
      -> Sem r (Validation [(QualifiedPath EntryPath, CreateError)] Entry)
    validateCopyOperation backend (CopyOperation old new) =
      checkCreateEntry backend newBackend force (qpPath new) <&> first \err -> [(QualifiedPath oldBackend (qpPath old ^. path), err)]
      where
        oldBackend = qpBackendName old
        newBackend = qpBackendName new

runCopyOperations :: (Members '[BackendEffect, Async] r) => SomeBackend -> [CopyOperation] -> Sem r ()
runCopyOperations backend operations = do
  let newEntries = qpPath . coQNew <$> operations
  void $ sequenceConcurrently $ map (writeEntry backend) newEntries
  --forM_ newEntries (writeEntry backend)

copyCmd
  :: (Members '[BackendEffect, Embed IO, Error CofferError, Error CopyResult, Async] r)
  => Config -> CopyOptions -> Sem r CopyResult
copyCmd
  config
  (CopyOptions
    dryRun
    oldQPath@(QualifiedPath oldBackendNameMb _)
    newQPath@(QualifiedPath newBackendNameMb _)
    force
  )
    = do
  oldBackend <- getBackend config oldBackendNameMb
  newBackend <- getBackend config newBackendNameMb
  operations <- buildCopyOperations oldBackend newBackend oldQPath newQPath force

  unless dryRun do
    runCopyOperations newBackend operations

  pure $ CPRSuccess dryRun $ getOperationPaths <$> operations

deleteCmd
  :: (Members '[BackendEffect, Embed IO, Error CofferError, Error DeleteResult] r)
  => Config -> DeleteOptions -> Sem r DeleteResult
deleteCmd config (DeleteOptions dryRun qPath@(QualifiedPath backendNameMb _) recursive) = do
  backend <- getBackend config backendNameMb
  getEntryOrDirThrow backend DRPathNotFound qPath >>= \case
    Left entry -> do
      unless dryRun do
        deleteEntry backend (entry ^. E.path)
      let qEntryPath = QualifiedPath backendNameMb (entry ^. E.path)

      pure $ DRSuccess dryRun [qEntryPath]
    Right dir
      | recursive -> do
          let entries = Dir.allEntries dir
          unless dryRun do
            forM_ entries \entry -> deleteEntry backend (entry ^. E.path)
          let qEntryPaths = entries ^.. each . E.path <&> QualifiedPath backendNameMb
          pure $ DRSuccess dryRun qEntryPaths
      | otherwise -> pure $ DRDirectoryFound qPath

tagCmd
  :: forall r
   . (Members '[BackendEffect, Embed IO, Error CofferError, Error TagResult] r)
  => Config -> TagOptions -> Sem r TagResult
tagCmd config (TagOptions qEntryPath@(QualifiedPath backendNameMb entryPath) tag delete) = do
  backend <- getBackend config backendNameMb
  readEntry backend entryPath >>= \case
    Nothing -> pure $ TREntryNotFound qEntryPath
    Just entry -> do
      nowUtc <- embed getCurrentTime
      updatedEntry <- updateEntry nowUtc entry
      void $ writeEntry backend updatedEntry
      pure $ TRSuccess (qEntryPath { qpPath = updatedEntry }) tag delete
  where
    updateEntry :: UTCTime -> Entry -> Sem r Entry
    updateEntry nowUtc entry =
      entry
        & E.dateModified .~ nowUtc
        & E.tags %%~ updateTags

    updateTags :: Set EntryTag -> Sem r (Set EntryTag)
    updateTags tags = do
      if delete then do
        when (Set.notMember tag tags) do
          throw $ TRTagNotFound tag
        pure $ Set.delete tag tags
      else do
        when (Set.member tag tags) do
          throw $ TRDuplicateTag tag
        pure $ Set.insert tag tags

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Checks if the path points to an existing directory.
pathIsDirectory :: forall r. Member BackendEffect r => SomeBackend -> EntryPath -> Sem r Bool
pathIsDirectory backend entryPath =
  listDirectoryContents backend (Path.entryPathAsPath entryPath) >>= \case
    Nothing -> pure False
    Just (DirectoryContents [] []) -> pure False
    Just _ -> pure True

-- | Checks if the path points to an existing entry.
pathIsEntry :: forall r. Member BackendEffect r => SomeBackend -> EntryPath -> Sem r Bool
pathIsEntry backend entryPath =
  readEntry backend entryPath <&> \case
    Nothing -> False
    Just _ -> True

-- | Returns the entry or directory that the path points to.
-- If the path doesn't exist at all, throws an error.
getEntryOrDirThrow
  :: (Members '[BackendEffect, Error CofferError, Error e] r)
  => SomeBackend -> (QualifiedPath Path -> e) -> QualifiedPath Path -> Sem r (Either Entry Directory)
getEntryOrDirThrow backend mkError qPath@(QualifiedPath _ path) = do
  getEntryOrDir backend path >>= \case
    Nothing -> throw (mkError qPath)
    Just entryOrDir -> pure entryOrDir

-- | Returns the entry or directory that the path points to.
-- If the path doesn't exist at all, returns `Nothing`.
getEntryOrDir
  :: forall r
   . (Members '[BackendEffect, Error CofferError] r)
  => SomeBackend -> Path -> Sem r (Maybe (Either Entry Directory))
getEntryOrDir backend path =
  tryGetEntry path >>= \case
    Just entry -> pure $ Just $ Left entry
    Nothing -> fmap Right <$> tryGetDir path
  where
    -- | Checks if the given path points to an entry and, if so, returns it.
    tryGetEntry :: Path -> Sem r (Maybe Entry)
    tryGetEntry path =
      case Path.pathAsEntryPath path of
        Left _ -> pure Nothing
        Right entryPath -> readEntry backend entryPath

    -- | Checks if the given path points to a directory.
    -- If so, returns all entries under the given directory and subdirectories.
    -- If the path doesn't exist OR is an entry, returns `Nothing`.
    tryGetDir :: Path -> Sem r (Maybe Directory)
    tryGetDir rootPath = do
      dir <- execStateT (go rootPath) Dir.emptyDir
      if dir == Dir.emptyDir
        then pure Nothing
        else pure $ Just dir
      where
        go :: Path -> StateT Directory (Sem r) ()
        go rootPath = do
          contents <- lift $ fromMaybe (DirectoryContents [] []) <$> listDirectoryContents backend rootPath
          -- TODO: run in parallel
          forM_ (contents ^. entryNames) \entryName -> do
            entry <- lift $ readEntry backend (Path.appendEntryName rootPath entryName)
            case entry of
              Just entry -> modify' (Dir.insertEntry entry)
              -- This entry has been concurrently deleted (e.g. by some other user) _while_ we're traversing the directory.
              -- We should just ignore it.
              Nothing -> pure ()

          forM_ (contents ^. directoryNames) \directoryName -> do
            let subdir = Path [directoryName]
            go (rootPath <> subdir)

-- | This function gets all entries, that are exist in given entry path.
--
-- Note: the root path @/@ cannot possibly be occupied by an entry,
-- therefore we skip the check for that path.
getEntriesInEntryPath
  :: forall r. Member BackendEffect r
  => SomeBackend -> EntryPath -> Sem r [EntryPath]
getEntriesInEntryPath backend entryPath = do
  let parentDirsExceptRoot = entryPath
        & Path.entryPathParentDirs
        & NE.toList
        -- Ignore paths that cannot possibly point to an entry
        <&> Path.pathAsEntryPath
        & rights

  filterM (pathIsEntry backend) parentDirsExceptRoot

checkCreateEntry
  :: forall r
   . (Member BackendEffect r)
  => SomeBackend -> Maybe BackendName -> Bool -> Entry -> Sem r (Validation CreateError Entry)
checkCreateEntry backend backendNameMb force entry = catchAndReturn act
  where
    act :: Sem (Error (Validation CreateError Entry) ': r) (Validation CreateError Entry)
    act = do
      let entryPath = entry ^. path
      whenM (pathIsDirectory backend entryPath) do
        throw $ Failure @_ @Entry (CEDestinationIsDirectory (QualifiedPath backendNameMb (entry ^. E.path)))

      -- When attempting to create an entry at, e.g., @/a/b/c@, the directories
      -- @/a@ and @/a/b@ will be implicitly created.
      --
      -- Checks whether those paths are already
      -- occupied by existing entries and, if so, fails.
      getEntriesInEntryPath backend entryPath >>= \case
        (clashed : _) ->
          throw $ Failure @_ @Entry (CEParentDirectoryIsEntry (QualifiedPath backendNameMb (entry ^. E.path), QualifiedPath backendNameMb clashed))
        [] -> pure ()

      when (not force) do
        whenM (pathIsEntry backend entryPath) do
          throw $ Failure @_ @Entry (CEEntryAlreadyExists (QualifiedPath backendNameMb (entry ^. E.path)))

      pure $ Success entry

getBackend
  :: forall r. Member (Error CofferError) r
  => Config -> Maybe BackendName -> Sem r SomeBackend
getBackend config backendNameMb = do
  let backendName = fromMaybe (mainBackend config) backendNameMb
  let backendsHashMap = backends config
  case backendsHashMap !? backendName of
    Just backend -> pure backend
    Nothing -> throw $ BackendNotFound backendName
