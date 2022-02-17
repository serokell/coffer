{-# LANGUAGE AllowAmbiguousTypes
           , TypeApplications
           , MultiParamTypeClasses
#-}

module Config where

import           Error               (CofferError);

import qualified Data.Text           as T;
import qualified Data.Text.IO        as TIO
import qualified Entry               as E
import qualified Data.HashMap.Strict as HS
import qualified Toml;

import           Polysemy.Error      (Error, errorToIOFinal)
import           Toml                (TomlCodec);

import           Polysemy
import           Backend             (BackendPacked (..)
                                     , Backend (..)
                                     , readSecret
                                     )
import           Backends            (backendPackedCodec, supportedBackends)


data Config =
  Config
  { backends :: HS.HashMap T.Text BackendPacked
  , mainBackend :: T.Text
  }
  deriving (Show)


configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.dimap hsToList listToHs
  (Toml.list (backendPackedCodec supportedBackends) "backend") Toml..= backends
  <*> Toml.text "main_backend" Toml..= mainBackend
  where hsToList hs = snd <$> HS.toList hs
        listToHs list = HS.fromList $ fmap (\y@(PackBackend x) -> (_name x, y)) list
