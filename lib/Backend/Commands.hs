-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Commands where

import Fmt ( pretty )
import Control.Lens hiding (view)
import CLI.Types
import Polysemy
import Control.Monad.State
import Data.Time (getCurrentTime, UTCTime, utctDay)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import GHC.Exts (Down(..), sortWith)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.Calendar.Month.Compat (pattern MonthDay)
import Control.Lens (view)
import Data.Functor (($>))
import Polysemy.Error (throw, Error)
import Control.Monad.Extra (whenM, whenJust)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set

import Backend (BackendEffect, listSecrets, readSecret, writeSecret, deleteSecret)
import Entry (Entry, Field, FieldKey, value, fields, dateModified, newEntry, newField, visibility, FieldVisibility(..), EntryTag)
import qualified Entry as E
import Coffer.Directory (Directory)
import qualified Coffer.Directory as Dir
import Coffer.Path (Path, mkPath, mkPathSegment, unPathSegment, entryPathName, EntryPath, pathSegments)
import qualified Coffer.Path as Path
import Error (CofferError(..))
import Coffer.Util (catchAndReturn)

runCommand
  :: (Member BackendEffect r, Member (Embed IO) r, Member (Error CofferError) r)
  => Command res -> Sem r res
runCommand = \case
  CmdView opts -> catchAndReturn $ viewCmd opts
  CmdCreate opts -> catchAndReturn $ createCmd opts
  CmdSetField opts -> catchAndReturn $ setFieldCmd opts
  CmdDeleteField opts -> deleteFieldCmd opts
  CmdFind opts -> findCmd opts
  CmdRename opts -> renameCmd opts
  CmdCopy opts -> catchAndReturn $ copyCmd opts
  CmdDelete opts -> catchAndReturn $ deleteCmd opts
  CmdTag opts -> catchAndReturn $ tagCmd opts

viewCmd
  :: (Member BackendEffect r, Member (Error CofferError) r, Member (Error ViewResult) r)
  => ViewOptions -> Sem r ViewResult
viewCmd (ViewOptions path fieldNameMb) = do
  getEntryOrDirThrow VRPathNotFound path >>= \case
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
  :: (Member BackendEffect r, Member (Embed IO) r, Member (Error CreateResult) r)
  => CreateOptions -> Sem r CreateResult
createCmd (CreateOptions entryPath _edit force tags fields privateFields) = do
  nowUtc <- embed getCurrentTime
  let
    mkField :: FieldVisibility -> FieldInfo -> (FieldKey, Field)
    mkField fv fi = (fiName fi, newField nowUtc (fiContents fi) & visibility .~ fv)

    allFields = (mkField Public <$> fields) <> (mkField Private <$> privateFields)

    entry = newEntry entryPath nowUtc
      & E.fields .~ HashMap.fromList allFields
      & E.tags .~ tags

  -- TODO: check that a directory does not exist at the given path.

  when (not force) do
    whenM (pathIsEntry entryPath) do
      throw $ CREntryAlreadyExists entryPath

  void $ writeSecret entry
  pure $ CRSuccess entry

setFieldCmd
  :: forall r
   . (Member BackendEffect r, Member (Embed IO) r, Member (Error SetFieldResult) r)
  => SetFieldOptions -> Sem r SetFieldResult
setFieldCmd (SetFieldOptions entryPath fieldName fieldContentsMb visibilityMb) = do
  readSecret entryPath >>= \case
    Nothing -> pure $ SFREntryNotFound entryPath
    Just entry -> do
      nowUtc <- embed getCurrentTime
      updatedEntry <- updateOrInsert nowUtc entry
      void $ writeSecret updatedEntry
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
  :: (Member BackendEffect r, Member (Embed IO) r)
  => DeleteFieldOptions -> Sem r DeleteFieldResult
deleteFieldCmd (DeleteFieldOptions path fieldName) = do
  readSecret path >>= \case
    Nothing -> pure $ DFREntryNotFound path
    Just entry -> do
      case entry ^. fields . at fieldName of
        Nothing -> pure $ DFRFieldNotFound fieldName
        _ -> do
          nowUtc <- embed getCurrentTime
          let newEntry = entry
                & fields . at fieldName .~ Nothing
                & dateModified .~ nowUtc
          void $ writeSecret newEntry
          pure $ DFRSuccess newEntry

findCmd :: (Member BackendEffect r, Member (Error CofferError) r) => FindOptions -> Sem r (Maybe Directory)
findCmd (FindOptions pathMb textMb sortMb filters filterFields) = do
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
            FilterFieldByValue substr -> substr `T.isInfixOf` (field ^. value)
            FilterFieldByDate op date -> matchDate op date (field ^. dateModified)

  let path = fromMaybe mempty pathMb
  dir <- getEntryOrDir path <&> \case
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

renameCmd :: RenameOptions -> Sem r RenameResult
renameCmd (RenameOptions _oldPath _newPath _force) = do
  -- TODO: reuse copy+delete operations
  undefined

data CopyOperation = CopyOperation
  { coOld :: Entry
  , coNew :: Entry
  }

copyCmd
  :: forall r
   . ( Member BackendEffect r
     , Member (Embed IO) r
     , Member (Error CofferError) r
     , Member (Error CopyResult) r
     )
  => CopyOptions -> Sem r CopyResult
copyCmd (CopyOptions oldPath newPath force) = do
  entryOrDir <- getEntryOrDirThrow CPRPathNotFound oldPath

  -- Build a list of operations to perform.
  nowUtc <- embed getCurrentTime
  operations <- fmap (setModifiedDate nowUtc) <$>
    case entryOrDir of
      Left entry -> copyEntry entry
      Right dir -> copyDir dir

  -- Checks that the paths we're trying to copy entries to aren't already occupied
  -- by an existing directory.
  checkCopyOperations operations
    do \(CopyOperation _ new) -> pathIsDirectory (new ^. E.path)
    do CPRDestinationIsDirectory . fmap getOperationPaths

  when (not force) do
    -- Checks that the paths we're trying to copy entries to aren't already occupied
    -- by an existing entry.
    checkCopyOperations operations
      do \(CopyOperation _ new) -> pathIsEntry (new ^. E.path)
      do CPREntryAlreadyExists . fmap getOperationPaths

  -- Write new entries.
  let newEntries = coNew <$> operations
  forM_ newEntries writeSecret
  pure $ CPRSuccess $ getOperationPaths <$> operations

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

    -- | Performs a check on all `CopyOperation`s and throws an error if any of checks fail.
    checkCopyOperations
      :: [CopyOperation]
      -> (CopyOperation -> Sem r Bool)
      -> (NonEmpty CopyOperation -> CopyResult)
      -> Sem r ()
    checkCopyOperations operations pred mkErr = do
      invalidOperations <- filterM pred operations
      whenJust (NE.nonEmpty invalidOperations) \invalidOperationsNE ->
        throw $ mkErr invalidOperationsNE

    getOperationPaths :: CopyOperation -> (EntryPath, EntryPath)
    getOperationPaths (CopyOperation old new) = (old ^. E.path, new ^. E.path)

deleteCmd
  :: (Member BackendEffect r, Member (Error CofferError) r, Member (Error DeleteResult) r)
  => DeleteOptions -> Sem r DeleteResult
deleteCmd (DeleteOptions path recursive) = do
  getEntryOrDirThrow DRPathNotFound path >>= \case
    Left entry -> deleteSecret (entry ^. E.path) $> DRSuccess [entry ^. E.path]
    Right dir
      | recursive -> do
          let entries = Dir.allEntries dir
          forM_ entries \entry -> deleteSecret (entry ^. E.path)
          pure $ DRSuccess $ entries ^.. each . E.path
      | otherwise -> pure $ DRDirectoryFound path

tagCmd
  :: forall r
   . (Member BackendEffect r, Member (Embed IO) r, Member (Error TagResult) r)
  => TagOptions -> Sem r TagResult
tagCmd (TagOptions entryPath tag delete) = do
  readSecret entryPath >>= \case
    Nothing -> pure $ TREntryNotFound entryPath
    Just entry -> do
      nowUtc <- embed getCurrentTime
      updatedEntry <- updateEntry nowUtc entry
      void $ writeSecret updatedEntry
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
pathIsDirectory :: forall r. Member BackendEffect r => EntryPath -> Sem r Bool
pathIsDirectory entryPath =
  listSecrets (Path.entryPathAsPath entryPath) >>= \case
    Nothing -> pure False
    Just [] -> pure False
    Just _ -> pure True

-- | Checks if the path points to an existing entry.
pathIsEntry :: forall r. Member BackendEffect r => EntryPath -> Sem r Bool
pathIsEntry entryPath =
  readSecret entryPath <&> \case
    Nothing -> False
    Just _ -> True

-- | Returns the entry or directory that the path points to.
-- If the path doesn't exist at all, throws an error.
getEntryOrDirThrow
  :: (Member BackendEffect r, Member (Error CofferError) r, Member (Error e) r)
  => (Path -> e) -> Path -> Sem r (Either Entry Directory)
getEntryOrDirThrow mkError path = do
  getEntryOrDir path >>= \case
    Nothing -> throw (mkError path)
    Just entryOrDir -> pure entryOrDir

-- | Returns the entry or directory that the path points to.
-- If the path doesn't exist at all, returns `Nothing`.
getEntryOrDir
  :: forall r
   . (Member BackendEffect r, Member (Error CofferError) r)
  => Path -> Sem r (Maybe (Either Entry Directory))
getEntryOrDir path =
  tryGetEntry path >>= \case
    Just entry -> pure $ Just $ Left entry
    Nothing -> fmap Right <$> tryGetDir path
  where
    -- | Checks if the given path points to an entry and, if so, returns it.
    tryGetEntry :: Path -> Sem r (Maybe Entry)
    tryGetEntry path =
      case Path.pathAsEntryPath path of
        Left _ -> pure Nothing
        Right entryPath -> readSecret entryPath

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
          secrets <- lift $ fromMaybe [] <$> listSecrets rootPath
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
                entry <- lift $ readSecret (Path.appendEntryName rootPath entryName)
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
