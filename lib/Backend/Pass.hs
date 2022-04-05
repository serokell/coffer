-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Pass
  ( PassBackend ) where
import Backend
import BackendName
import Coffer.Path
import Coffer.Path qualified as P
import Control.Exception (IOException)
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as T
import Effect.Fs
import Entry (Entry)
import Entry qualified as E
import Entry.Pass
import Error
import Fmt (build, fmt)
import Polysemy
import Polysemy.Error
import System.Directory qualified as D
import System.FilePath (makeRelative)
import System.IO.Error (isDoesNotExistError)
import System.Process.Typed
import Toml (TomlCodec)
import Toml qualified

data PassBackend =
  PassBackend
  { pbName :: BackendName
  , pbStoreDir :: FilePath
  , pbPassExe :: Maybe FilePath
  }
  deriving stock (Show)

passCodec :: TomlCodec PassBackend
passCodec =
  PassBackend
  <$> backendNameCodec "name" Toml..= pbName
  <*> Toml.string "store_dir" Toml..= pbStoreDir
  <*> Toml.dimatch fPathToT tToFPath (Toml.text "pass_exe") Toml..= pbPassExe
  where tToFPath = Just . T.unpack
        fPathToT :: Maybe String -> Maybe Text
        fPathToT a = a <&> T.pack


verifyPassStore
  :: Member (Error CofferError) r
  => Member (Embed IO) r
  => FilePath
  -> Sem r ()
verifyPassStore storeDir =
  res >>= \case
    Left e -> throw $ OtherError (show e & T.pack)
    Right (Just _) -> pure ()
    Right Nothing -> throw . OtherError $
      "You must first initialize the password store at: " <> T.pack storeDir
  where
    res = runError @FsError . runFsInIO $ do
      nodeExists (stringToPath $ storeDir <> "/.gpg-id")


wrapper
  :: Effects r
  => PassBackend
  -> [String]
  -> Maybe (StreamSpec 'STInput ())
  -> Sem r (ExitCode, ByteString, ByteString)
wrapper backend args input = do
  let passExe = pbPassExe backend
  let storeDir = pbStoreDir backend
  verifyPassStore storeDir

  proc (fromMaybe "pass" passExe) args
    & case input of
        Just a -> setStdin a
        Nothing -> setStdin nullStream
    & setEnv [("PASSWORD_STORE_DIR", storeDir)]
    & readProcess



pbWriteSecret
  :: Effects r => PassBackend -> Entry -> Sem r ()
pbWriteSecret backend entry = do
  let input =
        entry ^. re E.entry . re passTextPrism
        & encodeUtf8
        & BS.fromStrict

  (exitCode, _stdout, stderr) <-
    wrapper
      backend
      [ "insert"
      , "-mf"
      , entry ^. E.path & P.entryPathAsPath & build & fmt
      ]
      (Just $ byteStringInput input)

  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _i -> throw $ OtherError (T.decodeUtf8 $ BS.toStrict stderr)


pbReadSecret
  :: Effects r => PassBackend -> EntryPath -> Sem r (Maybe Entry)
pbReadSecret backend path = do
  (exitCode, stdout, stderr) <-
    wrapper
      backend
      [ "show"
      , path & P.entryPathAsPath & build & fmt
      ]
      Nothing

  case exitCode of
    ExitSuccess ->
      pure $ T.decodeUtf8 (BS.toStrict stdout) ^? passTextPrism . E.entry
    ExitFailure 1 ->
      pure Nothing
    ExitFailure _e ->
      throw $ OtherError (T.decodeUtf8 $ BS.toStrict stderr)

pbListSecrets
  :: Effects r => PassBackend -> Path -> Sem r (Maybe [Text])
pbListSecrets backend path = do
  let storeDir = pbStoreDir backend
  verifyPassStore storeDir

  let fpath = storeDir <> (path & build & fmt)
  contents <- runError (fromException @IOException $ D.listDirectory fpath)
              >>= (\case Left e ->
                           if | isDoesNotExistError e -> pure Nothing
                              | True -> throw $ OtherError (T.pack $ show e)
                         Right v -> pure $ Just v)
              <&> \a -> a <&> map (makeRelative fpath)

  pure $ contents <&> map (T.dropEnd 4 . T.pack)

pbDeleteSecret
  :: Effects r => PassBackend -> EntryPath -> Sem r ()
pbDeleteSecret backend path = do
  (exitCode, _stdout, stderr) <-
    wrapper
      backend
      [ "rm"
      , "-f"
      , path & P.entryPathAsPath & build & fmt
      ]
      Nothing

  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _e -> throw $ OtherError (T.decodeUtf8 $ BS.toStrict stderr)


instance Backend PassBackend where
  _name kvBackend = pbName kvBackend
  _codec = passCodec
  _writeSecret = pbWriteSecret
  _readSecret = pbReadSecret
  _listSecrets = pbListSecrets
  _deleteSecret = pbDeleteSecret
