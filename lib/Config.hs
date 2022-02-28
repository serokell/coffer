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
import           Data.Foldable       (toList)

import           Polysemy.Error      (Error, errorToIOFinal)
import           Toml                (TomlCodec);

import           Polysemy
import           Backend             (SomeBackend (..)
                                     , Backend (..)
                                     , readSecret
                                     )
import           Backends            (backendPackedCodec, supportedBackends)


data Config =
  Config
  { backends :: HS.HashMap T.Text SomeBackend
  , mainBackend :: T.Text
  }
  deriving (Show)


configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.dimap toList listToHs
  (Toml.list backendPackedCodec "backend") Toml..= backends
  <*> Toml.text "main_backend" Toml..= mainBackend
  where 
    listToHs list = HS.fromList $ fmap (\y@(SomeBackend x) -> (_name x, y)) list
