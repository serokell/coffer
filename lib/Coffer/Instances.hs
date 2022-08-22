-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Module with orphan instances
module Coffer.Instances () where

import Data.Data (Proxy(..))
import Data.OpenApi
import Data.Text (Text)
import Servant.Client (BaseUrl)

instance ToSchema BaseUrl where
  declareNamedSchema _ = pure $ NamedSchema Nothing $
    toSchema @Text Proxy
