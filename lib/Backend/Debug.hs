-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Debug
  ( DebugBackend
  , debugCodec
  ) where

import Backend
import Backends
import Coffer.Path
import Control.Lens
import Data.HashMap.Lazy qualified as HS
import Data.Text (Text)
import Data.Text qualified as T
import Entry (Entry)
import Polysemy
import Toml (TomlCodec, TomlEnv)
import Toml qualified
import Validation (Validation(Failure, Success))

data DebugBackend =
  DebugBackend
  { dSubType :: Text
  , dSubBackend :: SomeBackend
  }
  deriving stock (Show)

debugCodec :: TomlCodec DebugBackend
debugCodec = Toml.Codec input output
  where input :: TomlEnv DebugBackend
        input toml = case HS.lookup "sub_type" $ Toml.tomlPairs toml of
                       Just x ->
                         case Toml.backward Toml._Text x of
                           Right t ->
                             case supportedBackends t of
                               Right y ->
                                 let newToml = toml { Toml.tomlPairs =
                                                      Toml.tomlPairs toml
                                                      & HS.delete "sub_type"
                                                    }
                                 in
                                 case y newToml of
                                   Success b -> Success $ DebugBackend
                                                { dSubType = t
                                                , dSubBackend = b
                                                }
                                   Failure e -> Failure e
                               Left e ->
                                 Failure
                                 [ Toml.BiMapError "type" e
                                 ]
                           Left e ->
                             Failure
                             [ Toml.BiMapError "type" e
                             ]
                       Nothing ->
                         Failure
                         [ Toml.BiMapError "sub_type" $
                           Toml.ArbitraryError
                           "Debug backend doesn't have a `sub_type` key"
                         ]
        output :: DebugBackend -> Toml.TomlState DebugBackend
        output debugBackend =
          case dSubBackend debugBackend of
            SomeBackend (be :: a) -> do
              Toml.codecWrite (Toml.text "type") "debug"
              Toml.codecWrite (Toml.text "sub_type") (dSubType debugBackend)
              Toml.codecWrite (_codec @a) be
              pure debugBackend

dbWriteSecret
  :: Effects r => DebugBackend -> Entry -> Sem r ()
dbWriteSecret b entry = unSubBackend b $ \(SomeBackend backend) -> do
  embed $ putStrLn ("WriteSecret: \n" <> show entry)
  _writeSecret backend entry

dbReadSecret
  :: Effects r => DebugBackend -> EntryPath -> Sem r (Maybe Entry)
dbReadSecret b path = unSubBackend b $ \(SomeBackend backend) -> do
  embed $ putStrLn ("ReadSecret: " <> show path)
  _readSecret backend path >>= showPass "out: "

dbListSecrets
  :: Effects r => DebugBackend -> Path -> Sem r (Maybe [Text])
dbListSecrets b path = unSubBackend b $ \(SomeBackend backend) -> do
  embed $ putStrLn ("ListSecrets: " <> show path)
  _listSecrets backend path >>= showPass "out: "

dbDeleteSecret
  :: Effects r => DebugBackend -> EntryPath -> Sem r ()
dbDeleteSecret b path = unSubBackend b $ \(SomeBackend backend) -> do
  embed $ putStrLn ("DeleteSecret: " <> show path)
  _deleteSecret backend path

unSubBackend
  :: DebugBackend
  -> (SomeBackend -> a)
  -> a
unSubBackend b f = f (dSubBackend b)

showPass
  :: ( Member (Embed IO) r
     , Show a
     )
  => Text -> a -> Sem r a
showPass txt a = do
  let atxt = T.pack $ show a
  embed $ putStrLn (T.unpack $ txt <> atxt)
  pure a


instance Backend DebugBackend where
  _name debugBackend = (\(SomeBackend x) -> _name x) $ dSubBackend debugBackend
  _codec = debugCodec
  _writeSecret = dbWriteSecret
  _readSecret = dbReadSecret
  _listSecrets = dbListSecrets
  _deleteSecret = dbDeleteSecret
