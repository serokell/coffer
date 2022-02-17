module Error
  ( CofferError (..)
  ) where

import qualified Data.Text as T

data CofferError =
  EntryNotFound [T.Text]
  | MarshallingFailed
  | ConnectError
  | OtherError T.Text
  deriving (Show)
