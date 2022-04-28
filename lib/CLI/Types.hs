-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.Types where

import Coffer.Directory (Directory)
import Coffer.Path (EntryPath, Path, QualifiedPath)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Compat (Day, UTCTime, Year)
import Entry (Entry, EntryTag, Field, FieldContents, FieldName, FieldVisibility)

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
  | VRField FieldName Field
  | VRPathNotFound (QualifiedPath Path)
  | VRDirectoryNoFieldMatch (QualifiedPath Path) FieldName
  | VREntryNoFieldMatch (QualifiedPath EntryPath) FieldName

data CreateError
  = CEParentDirectoryIsEntry (QualifiedPath EntryPath, QualifiedPath EntryPath)
  | CEDestinationIsDirectory (QualifiedPath EntryPath)
  | CEEntryAlreadyExists (QualifiedPath EntryPath)

data CreateResult
  = CRSuccess Entry
  | CRCreateError CreateError

data SetFieldResult
  = SFRSuccess (QualifiedPath Entry)
  | SFREntryNotFound (QualifiedPath EntryPath)
  | SFRMissingFieldContents (QualifiedPath EntryPath)

data DeleteFieldResult
  = DFRSuccess Entry
  | DFREntryNotFound (QualifiedPath EntryPath)
  | DFRFieldNotFound FieldName

type RenameResult = CopyResult

data CopyResult
  = CPRSuccess [(QualifiedPath EntryPath, QualifiedPath EntryPath)]
  | CPRPathNotFound (QualifiedPath Path)
  | CPRMissingEntryName
  | CPRSamePath (QualifiedPath Path)
  | CPRCreateErrors [(QualifiedPath EntryPath, CreateError)]

data DeleteResult
  = DRSuccess [QualifiedPath EntryPath]
  | DRPathNotFound (QualifiedPath Path)
  | DRDirectoryFound (QualifiedPath Path)

data TagResult
  = TRSuccess Entry
  | TREntryNotFound (QualifiedPath EntryPath)
  | TRTagNotFound EntryTag
  | TRDuplicateTag EntryTag

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data ViewOptions = ViewOptions
  { voQPath :: QualifiedPath Path
  , voFieldName :: Maybe FieldName
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
  , sfoFieldName :: FieldName
  , sfoFieldContents :: Maybe FieldContents
  , sfoVisibility :: Maybe FieldVisibility
  }
  deriving stock Show

data DeleteFieldOptions = DeleteFieldOptions
  { dfoQPath :: QualifiedPath EntryPath
  , dfoFieldName :: FieldName
  }
  deriving stock Show

data FindOptions = FindOptions
  { foQPath :: Maybe (QualifiedPath Path)
  , foText :: Maybe Text
  , foSort :: Maybe (Sort, Direction)
  , foFilters :: [Filter]
  , foFilterFields :: [(FieldName, FilterField)]
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
  { fiName :: FieldName
  , fiContents :: FieldContents
  }
  deriving stock Show

data Direction = Asc | Desc
  deriving stock Show

data Sort
  = SortByEntryName
  | SortByEntryDate
  | SortByFieldContents FieldName
  | SortByFieldDate FieldName
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
  | FilterFieldByContents Text
  deriving stock Show
