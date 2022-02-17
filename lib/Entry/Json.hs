{-# LANGUAGE LambdaCase #-}
module Entry.Json where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import Entry
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

newtype JsonEntry = JsonEntry A.Value
  deriving (Show, Generic)

instance A.ToJSON JsonEntry
instance A.FromJSON JsonEntry

fieldConverter :: (Prism A.Value A.Value Field Field)
fieldConverter = prism' to from
  where to field =
          A.object
          [ "date_modified" A..= (field ^. dateModified)
          , "private" A..= (field ^. private)
          , "value" A..= (field ^. value)
          ]
        from (A.Object o) = do
          dateModified <- HS.lookup "date_modified" o
                               >>= \case A.String t -> Just t ; _ -> Nothing
                               >>= iso8601ParseM . T.unpack
          value <- HS.lookup "value" o
                     >>= \case A.String t -> Just t ; _ -> Nothing
          _private <- HS.lookup "private" o
                       >>= \case A.Bool b -> Just b ; _ -> Nothing

          pure $
              newField dateModified value
            & private .~ _private
        from _ = Nothing

instance EntryConvertible JsonEntry where
  entry = prism' to from
    where to entry =
            JsonEntry $ A.object
            [ "path" A..= T.intercalate "/" (entry ^. path)
            , "date_modified" A..= (entry ^. dateModified)
            , "master_field" A..= (entry ^. masterField)
            , "fields" A..= (HS.fromList . over (traverse . _1) getFieldKey . HS.toList  . HS.map (^. re fieldConverter) $ (entry ^. fields))
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
                                     >>= \case A.String t -> newFieldKey t ; _ -> Nothing
                _fields <- HS.lookup "fields" o
                  >>= (\case A.Object t -> Just t ; _ -> Nothing)
                  <&> HS.map (^? fieldConverter)
                  >>= (mapM (uncurry (liftA2 (,)) . over _1 newFieldKey) . HS.toList)
                  <&> HS.fromList

                pure $
                    newEntry path dateModified
                  & masterField .~ _masterField
                  & fields .~ _fields
          from _ = Nothing
