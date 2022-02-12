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
import           Control.Lens
import           Backend             (BackendPacked (..)
                                     , Backend (..)
                                     , readSecret, writeSecret
                                     , deleteSecret
                                     )
import           Backends            (backendPackedCodec, supportedBackends)

import Data.Time (UTCTime(UTCTime, utctDay, utctDayTime))
import Data.Time.Calendar.OrdinalDate (fromMondayStartWeek)


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

test :: IO ()
test = do
  text <- TIO.readFile "./test.toml"
  let config = Toml.decode configCodec text
  let retext = case config of
        Left a -> Left a
        Right b -> Right $ Toml.encode configCodec b

  case config of
    Right c -> do
      case HS.lookup (mainBackend c) (backends c) of
        Just (PackBackend b) -> do
          let runEffect = _runEffect b 
          print "what"
          secret <- runFinal . embedToFinal @IO . errorToIOFinal @CofferError
            $ runEffect $ do
            entry <- maybe undefined pure $ do
                  k1 <- E.newFieldKey "haei"
                  k2 <- E.newFieldKey "asdf"
                  tags <- mapM E.newFieldTag [ "password", "token", "secure" ]
                  let time = UTCTime { utctDay = fromMondayStartWeek 69 69 69, utctDayTime = 42 }


                  pure $ E.newEntry ["test"] time
                    & E.masterField ?~ k1
                    & E.fields .~ HS.fromList
                    [ ( k2
                      , E.newField time "123123"
                        & E.tags .~ tags
                      )
                    ]
            writeSecret entry
            readSecret ["test"] Nothing
            deleteSecret ["test"]
          print secret
          pure ()
        Nothing -> pure ()

      pure ()
    Left c -> pure ()

  print config
  print retext

  pure ()
