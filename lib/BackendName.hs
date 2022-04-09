-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module BackendName
  ( BackendName
  , backendNameCharSet
  , newBackendName
  , getBackendName
  , backendNameCodec
  ) where

import Coffer.Util (didimatch)
import Data.Aeson qualified as A
import Data.Hashable (Hashable)
import Data.Text qualified as T
import Fmt (Buildable)
import Toml qualified

newtype BackendName = UnsafeBackendName T.Text
  deriving stock (Show, Eq)
  deriving newtype (A.ToJSON, A.ToJSONKey, A.FromJSON, A.FromJSONKey, Hashable, Buildable)

backendNameCharSet :: [Char]
backendNameCharSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_;"

newBackendName :: T.Text -> Either T.Text BackendName
newBackendName t
  | T.null t =
      Left "Backend name should contain at least 1 character"
  | T.any (`notElem` backendNameCharSet) t =
      Left $ "Backend name can only contain the following characters: '" <> T.pack backendNameCharSet <> "'"
  | otherwise =
      Right $ UnsafeBackendName t

getBackendName :: BackendName -> T.Text
getBackendName (UnsafeBackendName t) = t

backendNameCodec :: Toml.Key -> Toml.TomlCodec BackendName
backendNameCodec backendNameKey =
  didimatch
    (Right . getBackendName)
    newBackendName
    (Toml.text backendNameKey)
