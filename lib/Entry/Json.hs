{-# LANGUAGE LambdaCase
           , OverloadedStrings
#-}
module Entry.Json where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import qualified Entry as E
import Control.Lens
import qualified Data.HashMap.Strict as HS
import Control.Monad.State (execState, runState, execStateT, lift)
import qualified Data.Text as T
import Control.Applicative (liftA2)
import qualified Control.Monad.Writer.Strict as T
import GHC.Generics (Generic)
import Data.Time (UTCTime(..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.Calendar.OrdinalDate (fromMondayStartWeek)
import qualified Data.Vector as V

newtype JsonEntry = JsonEntry A.Value
  deriving (Show, Generic)

instance A.ToJSON JsonEntry
instance A.FromJSON JsonEntry

fieldConverter :: (Prism A.Value A.Value E.Field E.Field)
fieldConverter = prism' to from
  where to field =
          A.object
          [ "date_modified" A..= (field ^. E.dateModified)
          , "private" A..= (field ^. E.private)
          , "value" A..= (field ^. E.value)
          , "tags" A..= (field ^. E.tags)
          ]
        from (A.Object o) = do
          dateModified <- HS.lookup "date_modified" o
                               >>= \case A.String t -> Just t ; _ -> Nothing
                               >>= iso8601ParseM . T.unpack
          value <- HS.lookup "value" o
                     >>= \case A.String t -> Just t ; _ -> Nothing
          _private <- HS.lookup "private" o
                        >>= \case A.Bool b -> Just b ; _ -> Nothing
          _tags <- HS.lookup "tags" o
                     >>= \case A.Array a -> Just a ; _ -> Nothing
                     >>= sequence . V.toList . V.map (\case A.String s -> E.newFieldTag s ; _ -> Nothing)

          pure $
              E.newField dateModified value
            & E.private .~ _private
            & E.tags .~ _tags
        from _ = Nothing

instance E.EntryConvertible JsonEntry where
  entry = prism' to from
    where to entry =
            JsonEntry $ A.object
            [ "path" A..= T.intercalate "/" (entry ^. E.path)
            , "date_modified" A..= (entry ^. E.dateModified)
            , "master_field" A..= (entry ^. E.masterField)
            , "fields" A..= (HS.fromList . over (traverse . _1) E.getFieldKey . HS.toList  . HS.map (^. re fieldConverter) $ (entry ^. E.fields))
            ]
          from (JsonEntry (A.Object o)) =
              do
                path <- HS.lookup "path" o
                  >>= (\case A.String t -> Just t ; _ -> Nothing)
                  <&> T.split (== '/')
                dateModified <- HS.lookup "date_modified" o
                  >>= \case A.String t -> Just t ; _ -> Nothing
                  >>= iso8601ParseM . T.unpack
                let _masterField = HS.lookup "master_field" o
                                     >>= \case A.String t -> E.newFieldKey t ; _ -> Nothing
                _fields <- HS.lookup "fields" o
                  >>= (\case A.Object t -> Just t ; _ -> Nothing)
                  <&> HS.map (^? fieldConverter)
                  >>= (mapM (uncurry (liftA2 (,)) . over _1 E.newFieldKey) . HS.toList)
                  <&> HS.fromList

                pure $
                    E.newEntry path dateModified
                  & E.masterField .~ _masterField
                  & E.fields .~ _fields
          from _ = Nothing

-- |
-- >>> import qualified Data.Aeson as A
-- >>> import Control.Lens
-- >>> import Vault.Entry
--
-- >>> testEntry <&> ((^. re entry) :: Entry -> JsonEntry) <&> A.encode >>= A.decode >>= (\x -> (JsonEntry x) ^? entry)
-- Just (Entry {_path = ["haei","aa"], _eDateModified = 0070-07-05 00:00:42 UTC, _masterField = Just (FieldKey "haei"), _fields = fromList [(FieldKey "asdf",Field {_fDateModified = 0070-07-05 00:00:42 UTC, _private = False, _value = "", _tags = []})]})

testEntry = do
  k1 <- E.newFieldKey "haei"
  k2 <- E.newFieldKey "asdf"
  tags <- mapM E.newFieldTag [ "password", "token", "secure" ]
  let time = UTCTime { utctDay = fromMondayStartWeek 69 69 69, utctDayTime = 42 }


  pure $ E.newEntry ["haei", "aa"] time
         & E.masterField ?~ k1
         & E.fields .~ HS.fromList
         [ ( k2
           , E.newField time ""
             & E.tags .~ tags
           )
         ]
  -- execStateT
  --   (do
  --     E.masterField .= Just k1
  --     E.fields .= HS.fromList [ (k2, E.newField time "") ]
  --   ) (E.newEntry ["haei", "aa"] time)
