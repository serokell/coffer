-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.Types where

import Data.Text (Text)
import Data.Time.Compat (Day, UTCTime , Year)
import Data.Time.Calendar.Month.Compat (Month)
import Entry (FieldKey, Field, Entry, FieldVisibility, EntryTag, FieldValue)
import Coffer.Directory (Directory)
import Coffer.Path (Path, EntryPath, QualifiedPath)
import Data.Set (Set)

data Options = Options
  { oConfigPathMb :: Maybe FilePath
  , oSomeCommand :: SomeCommand
  }
  deriving stock Show

data Command res where
  CmdView :: ViewOptions -> Command ViewResult
  CmdCreate :: CreateOptions -> Command CreateResult
  CmdSetField :: SetFieldOptions -> Command SetFieldResult
  CmdDeleteField :: DeleteFieldOptions -> Command DeleteFieldResult
  CmdFind :: FindOptions -> Command (Maybe Directory)
  CmdRename :: RenameOptions -> Command RenameResult
  CmdCopy :: CopyOptions -> Command CopyResult
  CmdDelete :: DeleteOptions -> Command DeleteResult
  CmdTag :: TagOptions -> Command TagResult

deriving stock instance Show (Command res)

data SomeCommand where
  SomeCommand :: Command res -> SomeCommand

deriving stock instance Show SomeCommand

----------------------------------------------------------------------------
-- Command results
----------------------------------------------------------------------------

data ViewResult
  = VRDirectory Directory
  | VREntry Entry
  | VRField FieldKey Field
  | VRPathNotFound Path
  | VRDirectoryNoFieldMatch Path FieldKey
  | VREntryNoFieldMatch EntryPath FieldKey

data CreateError
  = CEParentDirectoryIsEntry (Entry, EntryPath)
  | CEDestinationIsDirectory Entry
  | CEEntryAlreadyExists Entry

data CreateResult
  = CRSuccess Entry
  | CRCreateError CreateError

data SetFieldResult
  = SFRSuccess Entry
  | SFREntryNotFound EntryPath
  | SFRMissingFieldContents EntryPath

data DeleteFieldResult
  = DFRSuccess Entry
  | DFREntryNotFound EntryPath
  | DFRFieldNotFound FieldKey

type RenameResult = CopyResult

data CopyResult
  = CPRSuccess [(EntryPath, EntryPath)]
  | CPRPathNotFound Path
  | CPRMissingEntryName
  | CPRCreateErrors [(EntryPath, CreateError)]

data DeleteResult
  = DRSuccess [EntryPath]
  | DRPathNotFound Path
  | DRDirectoryFound Path

data TagResult
  = TRSuccess Entry
  | TREntryNotFound EntryPath
  | TRTagNotFound EntryTag
  | TRDuplicateTag EntryTag

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data ViewOptions = ViewOptions
  { voQPath :: QualifiedPath Path
  , voFieldName :: Maybe FieldKey
  }
  deriving stock Show

data CreateOptions = CreateOptions
  { coQPath :: QualifiedPath EntryPath
  , coEdit :: Bool
  , coForce :: Bool
  , coTags :: Set EntryTag
  , coFields :: [FieldInfo]
  , coPrivateFields :: [FieldInfo]
  }
  deriving stock Show

data SetFieldOptions = SetFieldOptions
  { sfoQPath :: QualifiedPath EntryPath
  , sfoFieldName :: FieldKey
  , sfoFieldContents :: Maybe FieldValue
  , sfoVisibility :: Maybe FieldVisibility
  }
  deriving stock Show

data DeleteFieldOptions = DeleteFieldOptions
  { dfoQPath :: QualifiedPath EntryPath
  , dfoFieldName :: FieldKey
  }
  deriving stock Show

data FindOptions = FindOptions
  { foQPath :: Maybe (QualifiedPath Path)
  , foText :: Maybe Text
  , foSort :: Maybe (Sort, Direction)
  , foFilters :: [Filter]
  , foFilterFields :: [(FieldKey, FilterField)]
  }
  deriving stock Show

data RenameOptions = RenameOptions
  { roDryRun :: Bool
  , roQOldPath :: QualifiedPath Path
  , roQNewPath :: QualifiedPath Path
  , roForce :: Bool
  }
  deriving stock Show

data CopyOptions = CopyOptions
  { cpoDryRun :: Bool
  , cpoQOldPath :: QualifiedPath Path
  , cpoQNewPath :: QualifiedPath Path
  , cpoForce :: Bool
  }
  deriving stock Show

data DeleteOptions = DeleteOptions
  { doDryRun :: Bool
  , doQPath :: QualifiedPath Path
  , doRecursive :: Bool
  }
  deriving stock Show

data TagOptions = TagOptions
  { toQPath :: QualifiedPath EntryPath
  , toTagName :: EntryTag
  , toDelete :: Bool
  }
  deriving stock Show

----------------------------------------------------------------------------
-- Option arguments
----------------------------------------------------------------------------

data FieldInfo = FieldInfo
  { fiName :: FieldKey
  , fiContents :: FieldValue
  }
  deriving stock Show

data Direction = Asc | Desc
  deriving stock Show

data Sort
  = SortByEntryName
  | SortByEntryDate
  | SortByFieldValue FieldKey
  | SortByFieldDate FieldKey
  deriving stock Show

data FilterOp = OpGT | OpGTE | OpLT | OpLTE | OpEQ
  deriving stock Show

data FilterDate
  = FDYear Year
  | FDMonth Month
  | FDDay Day
  | FDTime UTCTime
  deriving stock Show

data Filter
  = FilterByDate FilterOp FilterDate
  | FilterByName Text
  deriving stock Show

data FilterField
  = FilterFieldByDate FilterOp FilterDate
  | FilterFieldByValue Text
  deriving stock Show
