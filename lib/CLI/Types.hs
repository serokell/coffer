module CLI.Types where

import Data.Text (Text)
import Data.Time.Compat (Day, UTCTime , Year)
import Data.Time.Calendar.Month.Compat (Month)
import Entry (FieldKey, FieldVisibility, EntryTag)
import Coffer.Path (Path, EntryPath)

data Command
  = CmdView ViewOptions
  | CmdCreate CreateOptions
  | CmdSetField SetFieldOptions
  | CmdDeleteField DeleteFieldOptions
  | CmdFind FindOptions
  | CmdRename RenameOptions
  | CmdCopy CopyOptions
  | CmdDelete DeleteOptions
  | CmdTag TagOptions
  deriving stock Show

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

data ViewOptions = ViewOptions
  { voPath :: Path
  , voFieldName :: Maybe FieldKey
  }
  deriving stock Show

data CreateOptions = CreateOptions
  { coPath :: EntryPath
  , coEdit :: Bool
  , coForce :: Bool
  , coTags :: [EntryTag]
  , coFields :: [FieldInfo]
  , coPrivateFields :: [FieldInfo]
  }
  deriving stock Show

data SetFieldOptions = SetFieldOptions
  { sfoPath :: EntryPath
  , sfoFieldName :: FieldKey
  , sfoFieldContents :: Maybe Text
  , sfoVisibility :: Maybe FieldVisibility
  }
  deriving stock Show

data DeleteFieldOptions = DeleteFieldOptions
  { dfoPath :: EntryPath
  , dfoFieldName :: FieldKey
  }
  deriving stock Show

data FindOptions = FindOptions
  { foPath :: Maybe Path
  , foText :: Maybe Text
  , foSort :: Maybe (Sort, Direction)
  , foFilters :: [Filter]
  , foFilterFields :: [(FieldKey, FilterField)]
  }
  deriving stock Show

data RenameOptions = RenameOptions
  { roOldPath :: Path
  , roNewPath :: Path
  , roForce :: Bool
  }
  deriving stock Show

data CopyOptions = CopyOptions
  { cpoOldPath :: Path
  , cpoNewPath :: Path
  , cpoForce :: Bool
  }
  deriving stock Show

data DeleteOptions = DeleteOptions
  { doPath :: Path
  , doRecursive :: Bool
  }
  deriving stock Show

data TagOptions = TagOptions
  { toPath :: EntryPath
  , toTagName :: EntryTag
  , toDelete :: Bool
  }
  deriving stock Show

----------------------------------------------------------------------------
-- Option arguments
----------------------------------------------------------------------------

data FieldInfo = FieldInfo
  { fiName :: FieldKey
  , fiContents :: Text
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
