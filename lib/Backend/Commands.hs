-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Commands where

import Backend (BackendEffect, SomeBackend, deleteSecret, listSecrets, readSecret, writeSecret)
import BackendName (BackendName)
import CLI.Types
import Coffer.Directory (Directory)
import Coffer.Directory qualified as Dir
import Coffer.Path
  (EntryPath, Path, QualifiedPath(QualifiedPath, qpBackendName, qpPath), entryPathName, mkPath,
  mkPathSegment, pathSegments, unPathSegment)
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
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.Calendar.Month.Compat (pattern MonthDay)
import Entry
  (Entry, EntryTag, Field, FieldKey, FieldVisibility(..), dateModified, fieldValue, fields,
  newEntry, newField, path, value, visibility)
import Entry qualified as E
import Error (CofferError(..))
import Fmt (pretty)
import GHC.Exts (Down(..), sortWith)
import Polysemy
import Polysemy.Error (Error, throw)
import Validation (Validation(Failure, Success))

runCommand
  :: (Member BackendEffect r, Member (Embed IO) r, Member (Error CofferError) r)
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
  :: (Member BackendEffect r, Member (Error CofferError) r, Member (Error ViewResult) r)
  => Config -> ViewOptions -> Sem r ViewResult
viewCmd config (ViewOptions (QualifiedPath backendNameMb path) fieldNameMb) = do
  backend <- getBackend config backendNameMb
  getEntryOrDirThrow backend VRPathNotFound path >>= \case
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
            & maybe (VRDirectoryNoFieldMatch path fieldName) VRDirectory
    Left entry ->
      case fieldNameMb of
        Nothing -> pure $ VREntry entry
        Just fieldName ->
          case entry ^? fields . ix fieldName of
            Just field -> pure $ VRField fieldName field
            Nothing -> pure $ VREntryNoFieldMatch (entry ^. E.path) fieldName

createCmd
  :: forall r
   . (Member BackendEffect r, Member (Error CofferError) r, Member (Embed IO) r, Member (Error CreateResult) r)
  => Config -> CreateOptions -> Sem r CreateResult
createCmd config (CreateOptions (QualifiedPath backendNameMb entryPath) _edit force tags fields privateFields) = do
  backend <- getBackend config backendNameMb
  nowUtc <- embed getCurrentTime
  let
    mkField :: FieldVisibility -> FieldInfo -> (FieldKey, Field)
    mkField fv fi = (fiName fi, newField nowUtc (fiContents fi) & visibility .~ fv)

    allFields = (mkField Public <$> fields) <> (mkField Private <$> privateFields)

    entry = newEntry entryPath nowUtc
      & E.fields .~ HashMap.fromList allFields
      & E.tags .~ tags

  checkCreateEntry backend force entry >>= \case
    Failure error -> throw $ CRCreateError error
    Success entry -> do
      void $ writeSecret backend entry
      pure $ CRSuccess entry

setFieldCmd
  :: forall r
   . (Member BackendEffect r, Member (Error CofferError) r, Member (Embed IO) r, Member (Error SetFieldResult) r)
  => Config -> SetFieldOptions -> Sem r SetFieldResult
setFieldCmd config (SetFieldOptions (QualifiedPath backendNameMb entryPath) fieldName fieldContentsMb visibilityMb) = do
  backend <- getBackend config backendNameMb
  readSecret backend entryPath >>= \case
    Nothing -> pure $ SFREntryNotFound entryPath
    Just entry -> do
      nowUtc <- embed getCurrentTime
      updatedEntry <- updateOrInsert nowUtc entry
      void $ writeSecret backend updatedEntry
      pure $ SFRSuccess updatedEntry
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
          & value %~ do \currentValue -> fromMaybe currentValue fieldContentsMb
          & visibility %~ do \currentPrivate -> fromMaybe currentPrivate visibilityMb
      Nothing ->
        -- The field does not yet exist, insert a new one.
        case fieldContentsMb of
          Just fieldContents -> pure $ Just $ newField nowUtc fieldContents
            & visibility %~ do \currentPrivate -> fromMaybe currentPrivate visibilityMb
          -- If we're trying to insert a new field, but the user has not specified
          -- what the field contents should be, return an error.
          Nothing -> throw $ SFRMissingFieldContents entryPath

deleteFieldCmd
  :: (Member BackendEffect r, Member (Error CofferError) r, Member (Embed IO) r)
  => Config -> DeleteFieldOptions -> Sem r DeleteFieldResult
deleteFieldCmd config (DeleteFieldOptions (QualifiedPath backendNameMb path) fieldName) = do
  backend <- getBackend config backendNameMb
  readSecret backend path >>= \case
    Nothing -> pure $ DFREntryNotFound path
    Just entry -> do
      case entry ^. fields . at fieldName of
        Nothing -> pure $ DFRFieldNotFound fieldName
        _ -> do
          nowUtc <- embed getCurrentTime
          let newEntry = entry
                & fields . at fieldName .~ Nothing
                & dateModified .~ nowUtc
          void $ writeSecret backend newEntry
          pure $ DFRSuccess newEntry

findCmd :: (Member BackendEffect r, Member (Error CofferError) r) => Config -> FindOptions -> Sem r (Maybe Directory)
findCmd config (FindOptions qPathMb textMb sortMb filters filterFields) = do
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

    applyFilterField :: Entry -> (FieldKey, FilterField) -> Bool
    applyFilterField e (fieldName, filter) =
      case e ^? fields . ix fieldName of
        Nothing -> False
        Just field ->
          case filter of
            FilterFieldByValue substr -> substr `T.isInfixOf` (field ^. value . fieldValue)
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
          && all (applyFilterField e) filterFields
        )
      & Dir.mapDir (\entries ->
          case sortMb of
            Nothing -> entries
            Just (SortByEntryName, direction) ->
              sortWith' direction (view $ E.path . to entryPathName) entries
            Just (SortByFieldValue fieldName, direction) -> do
              let getFieldValue e = e ^? fields . ix fieldName . value
              sortWith' direction getFieldValue entries
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
      . value
      . fieldValue
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
  :: forall r.
     ( Member BackendEffect r
     , Member (Embed IO) r
     , Member (Error CofferError) r
     , Member (Error RenameResult) r
     )
  => Config -> RenameOptions -> Sem r RenameResult
renameCmd config (RenameOptions dryRun (QualifiedPath oldBackendNameMb oldPath) (QualifiedPath newBackendNameMb newPath) force) = do
  oldBackend <- getBackend config oldBackendNameMb
  newBackend <- getBackend config newBackendNameMb
  operations <- buildCopyOperations oldBackend newBackend oldPath newPath force

  unless dryRun do
    runCopyOperations newBackend operations

  -- we don't want to delete clashed entries if renaming is forced.
  let pathsToDelete =
        flip filter operations \(CopyOperation old _) ->
          none (\(CopyOperation _ new) -> old ^. path == new ^. path) operations

  -- If directory/entry was successfully copied, then we can delete old directory/entry without delete errors.
  unless dryRun do
    forM_ pathsToDelete \(CopyOperation old _) -> do
      let qPath = QualifiedPath oldBackendNameMb (Path.entryPathAsPath (old ^. path))
      void $ catchAndReturn $ deleteCmd config (DeleteOptions dryRun qPath False)

  pure $ CPRSuccess $ getOperationPaths <$> operations

data CopyOperation = CopyOperation
  { coOld :: Entry
  , coNew :: Entry
  }
  deriving stock Show

getOperationPaths :: CopyOperation -> (EntryPath, EntryPath)
getOperationPaths (CopyOperation old new) = (old ^. E.path, new ^. E.path)

buildCopyOperations
  :: forall r
   . ( Member BackendEffect r
     , Member (Embed IO) r
     , Member (Error CofferError) r
     , Member (Error CopyResult) r
     )
  => SomeBackend -> SomeBackend -> Path -> Path -> Bool -> Sem r [CopyOperation]
buildCopyOperations oldBackend newBackend oldPath newPath force = do
  entryOrDir <- getEntryOrDirThrow oldBackend CPRPathNotFound oldPath

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
        Right newEntryPath -> pure [CopyOperation entry (entry & E.path .~ newEntryPath)]

    copyDir :: Directory -> Sem r [CopyOperation]
    copyDir dir = do
      forM (Dir.allEntries dir) \entry -> do
        -- Get the entry's parent dir, remove the `oldPath` prefix from it,
        -- and add the `newPath` prefix.
        newEntry <- entry & E.path . Path.entryPathParentDir %%~ \parentDir ->
          case Path.replacePathPrefix oldPath newPath parentDir of
            Just newParentDir -> pure newParentDir
            Nothing -> throw $ OtherError $ T.unlines
              [ "Internal error:"
              , "Expected path: '" <> pretty (entry ^. E.path) <> "'"
              , "To have the prefix: '" <> pretty oldPath <> "'"
              ]
        pure $ CopyOperation entry newEntry

    setModifiedDate :: UTCTime -> CopyOperation -> CopyOperation
    setModifiedDate nowUtc (CopyOperation old new) =
      CopyOperation old (new & dateModified .~ nowUtc)

    -- | Performs a check on `CopyOperation` and returns @Failure@ if any of checks fail.
    validateCopyOperation :: SomeBackend -> CopyOperation -> Sem r (Validation [(EntryPath, CreateError)] Entry)
    validateCopyOperation backend (CopyOperation old new) =
      checkCreateEntry backend force new <&> first \err -> [(old ^. path, err)]

runCopyOperations :: (Member BackendEffect r) => SomeBackend -> [CopyOperation] -> Sem r ()
runCopyOperations backend operations = do
  let newEntries = coNew <$> operations
  forM_ newEntries (writeSecret backend)

copyCmd
  :: ( Member BackendEffect r
     , Member (Embed IO) r
     , Member (Error CofferError) r
     , Member (Error CopyResult) r
     )
  => Config -> CopyOptions -> Sem r CopyResult
copyCmd config (CopyOptions dryRun (QualifiedPath oldBackendNameMb oldPath) (QualifiedPath newBackendNameMb newPath) force) = do
  oldBackend <- getBackend config oldBackendNameMb
  newBackend <- getBackend config newBackendNameMb
  operations <- buildCopyOperations oldBackend newBackend oldPath newPath force

  unless dryRun do
    runCopyOperations newBackend operations

  pure $ CPRSuccess $ getOperationPaths <$> operations

deleteCmd
  :: (Member BackendEffect r, Member (Error CofferError) r, Member (Error DeleteResult) r)
  => Config -> DeleteOptions -> Sem r DeleteResult
deleteCmd config (DeleteOptions dryRun (QualifiedPath backendNameMb path) recursive) = do
  backend <- getBackend config backendNameMb
  getEntryOrDirThrow backend DRPathNotFound path >>= \case
    Left entry -> do
      unless dryRun do
        deleteSecret backend (entry ^. E.path)
      pure $ DRSuccess [entry ^. E.path]
    Right dir
      | recursive -> do
          let entries = Dir.allEntries dir
          unless dryRun do
            forM_ entries \entry -> deleteSecret backend (entry ^. E.path)
          pure $ DRSuccess $ entries ^.. each . E.path
      | otherwise -> pure $ DRDirectoryFound path

tagCmd
  :: forall r
   . (Member BackendEffect r, Member (Error CofferError) r, Member (Embed IO) r, Member (Error TagResult) r)
  => Config -> TagOptions -> Sem r TagResult
tagCmd config (TagOptions (QualifiedPath backendNameMb entryPath) tag delete) = do
  backend <- getBackend config backendNameMb
  readSecret backend entryPath >>= \case
    Nothing -> pure $ TREntryNotFound entryPath
    Just entry -> do
      nowUtc <- embed getCurrentTime
      updatedEntry <- updateEntry nowUtc entry
      void $ writeSecret backend updatedEntry
      pure $ TRSuccess updatedEntry
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
  listSecrets backend (Path.entryPathAsPath entryPath) >>= \case
    Nothing -> pure False
    Just [] -> pure False
    Just _ -> pure True

-- | Checks if the path points to an existing entry.
pathIsEntry :: forall r. Member BackendEffect r => SomeBackend -> EntryPath -> Sem r Bool
pathIsEntry backend entryPath =
  readSecret backend entryPath <&> \case
    Nothing -> False
    Just _ -> True

-- | Returns the entry or directory that the path points to.
-- If the path doesn't exist at all, throws an error.
getEntryOrDirThrow
  :: (Member BackendEffect r, Member (Error CofferError) r, Member (Error e) r)
  => SomeBackend -> (Path -> e) -> Path -> Sem r (Either Entry Directory)
getEntryOrDirThrow backend mkError path = do
  getEntryOrDir backend path >>= \case
    Nothing -> throw (mkError path)
    Just entryOrDir -> pure entryOrDir

-- | Returns the entry or directory that the path points to.
-- If the path doesn't exist at all, returns `Nothing`.
getEntryOrDir
  :: forall r
   . (Member BackendEffect r, Member (Error CofferError) r)
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
        Right entryPath -> readSecret backend entryPath

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
          secrets <- lift $ fromMaybe [] <$> listSecrets backend rootPath
          -- TODO: run in parallel
          forM_ secrets \secret -> do
            -- We need to find out whether `secret` is a directory name or an entry name.
            --
            -- Here, we rely on the fact that Vault returns directory names suffixed by `/`.
            -- and entry names without any suffix.
            --
            -- If `mkPathSegment` succeeds, then `secret` was not suffixed by `/`,
            -- and thus it's an entry name.
            -- Otherwise, it's a directory name (and `mkPath` should succeed).
            case (mkPathSegment secret, mkPath secret) of
              (Right entryName, _) -> do
                entry <- lift $ readSecret backend (Path.appendEntryName rootPath entryName)
                case entry of
                  Just entry -> modify' (Dir.insertEntry entry)
                  Nothing -> pure ()

              (_, Right subdir) -> go (rootPath <> subdir)
              _ -> lift $ throw $ OtherError $ T.unlines
                    [ "Internal error:"
                    , "Backend returned a secret that is not a valid\
                      \ entry or directory name."
                    , "Got: '" <> secret <> "'."
                    ]

-- | This function gets all entries, that are exist in given entry path.
--
-- Note: the root path @/@ cannot possibly be occupied by an entry,
-- therefore we skip the check for that path.
getEntriesInEntryPath :: forall r. Member BackendEffect r => SomeBackend -> EntryPath -> Sem r [EntryPath]
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
  => SomeBackend -> Bool -> Entry -> Sem r (Validation CreateError Entry)
checkCreateEntry backend force entry = catchAndReturn act
  where
    act :: Sem (Error (Validation CreateError Entry) ': r) (Validation CreateError Entry)
    act = do
      let entryPath = entry ^. path
      whenM (pathIsDirectory backend entryPath) do
        throw $ Failure @_ @Entry (CEDestinationIsDirectory entry)

      -- When attempting to create an entry at, e.g., @/a/b/c@, the directories
      -- @/a@ and @/a/b@ will be implicitly created.
      --
      -- Checks whether those paths are already
      -- occupied by existing entries and, if so, fails.
      getEntriesInEntryPath backend entryPath >>= \case
        (clashed : _) -> throw $ Failure @_ @Entry (CEParentDirectoryIsEntry (entry, clashed))
        [] -> pure ()

      when (not force) do
        whenM (pathIsEntry backend entryPath) do
          throw $ Failure @_ @Entry (CEEntryAlreadyExists entry)

      pure $ Success entry

getBackend :: forall r. Member (Error CofferError) r => Config -> Maybe BackendName -> Sem r SomeBackend
getBackend config backendNameMb = do
  let backendName = fromMaybe (mainBackend config) backendNameMb
  let backendsHashMap = backends config
  case backendsHashMap !? backendName of
    Just backend -> pure backend
    Nothing -> throw $ BackendNotFound backendName
