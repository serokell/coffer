module Error
  ( CofferError (..)
  ) where

import qualified Data.Text as T

data CofferError =
  MarshallingFailed
  | ConnectError
  | OtherError T.Text
  deriving (Show)
