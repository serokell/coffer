-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Coffer.Util
  ( catchAndReturn
  , didimatch
  , MParser
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Polysemy (Sem)
import Polysemy.Error (Error, runError)
import Text.Megaparsec (Parsec)
import Toml (TomlCodec)
import Toml qualified
import Validation (Validation(Failure, Success))

----------------------------------------------------------------------------
-- Polysemy helpers
----------------------------------------------------------------------------

-- | Takes an action that may return an @e@ or throw an @e@,
-- and turns it into an action that always returns an @e@.
catchAndReturn :: forall e r. Sem (Error e ': r) e -> Sem r e
catchAndReturn action =
  either id id <$> runError @e action

----------------------------------------------------------------------------
-- Tomland helpers
----------------------------------------------------------------------------

{-# INLINE didimatch #-}
didimatch
  :: (b -> Either Text a)  -- ^ Mapper for consumer
  -> (a -> Either Text b)  -- ^ Mapper for producer
  -> TomlCodec a  -- ^ Source 'Codec' object
  -> TomlCodec b  -- ^ Target 'Codec' object
didimatch matchB matchA codec = Toml.Codec
  { Toml.codecRead = \t -> case Toml.codecRead codec t of
      Success a ->
        case matchA a of
          Left err -> Failure [Toml.ParseError $ Toml.TomlParseError err]
          Right b -> Success b
      Failure b -> Failure b
  , Toml.codecWrite = \b -> do
      a <- Toml.eitherToTomlState $ matchB b
      a' <- Toml.codecWrite codec a
      Toml.eitherToTomlState $ matchA a'
  }

----------------------------------------------------------------------------
-- Megaparsec helpers
----------------------------------------------------------------------------

type MParser = Parsec Void Text
