-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE AllowAmbiguousTypes
           , TypeApplications
           , MultiParamTypeClasses
#-}

module Config where

;

import qualified Data.Text           as T;
import qualified Data.HashMap.Strict as HS
import qualified Toml;
import           Data.Foldable       (toList)

import           Toml                (TomlCodec);

import           Backend             (SomeBackend (..)
                                     , Backend (..)

                                     )
import           Backends            (backendPackedCodec)

data Config =
  Config
  { backends :: HS.HashMap T.Text SomeBackend
  , mainBackend :: T.Text
  }
  deriving stock (Show)


configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.dimap toList listToHs
  (Toml.list backendPackedCodec "backend") Toml..= backends
  <*> Toml.text "main_backend" Toml..= mainBackend
  where
    listToHs list = HS.fromList $ fmap (\y@(SomeBackend x) -> (_name x, y)) list
