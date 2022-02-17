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
          , "value" A..= (field ^. value)
          ]
        from (A.Object o) =
          (execStateT $
            do
              _dateModified <- lift $ HS.lookup "date_modified" o
                >>= \case A.String t -> Just t ; _ -> Nothing
                >>= iso8601ParseM . T.unpack
              _value <- lift $ HS.lookup "value" o
                >>= \case A.String t -> Just t ; _ -> Nothing

              dateModified .= _dateModified
              value .= _value) emptyField
        from _ = Nothing

instance EntryConvertible JsonEntry where
  entry = prism' to from
    where to entry =
            JsonEntry $ A.object
            [ "path" A..= T.intercalate "/" (entry ^. path)
            , "date_modified" A..= (entry ^. dateModified)
            , "master_field" A..= over _2 (^. re fieldConverter) (entry ^. masterField)
            , "fields" A..= (HS.fromList . over (traverse . _1) getFieldKey . HS.toList  . HS.map (^. re fieldConverter) $ (entry ^. fields))
            ]
          from (JsonEntry (A.Object o)) =
            (execStateT $
              do
                _path <- lift $ HS.lookup "path" o
                  >>= (\case A.String t -> Just t ; _ -> Nothing)
                  <&> T.split (== '/')
                _dateModified <- lift $ HS.lookup "date_modified" o
                  >>= \case A.String t -> Just t ; _ -> Nothing
                  >>= iso8601ParseM . T.unpack
                _masterField <- lift $ HS.lookup "master_field" o
                  >>= \case A.Array t -> Just t ; _ -> Nothing
                  >>= uncurry (liftA2 (,)) .
                      \t -> (t ^? traversed.index 0 >>= \case A.String t -> newFieldKey t ; _ -> Nothing, t ^? traversed.index 1 >>= (^? fieldConverter))
                _fields <- lift $ HS.lookup "fields" o
                  >>= (\case A.Object t -> Just t ; _ -> Nothing)
                  <&> HS.map (^? fieldConverter)
                  >>= (mapM (uncurry (liftA2 (,)) . over _1 newFieldKey) . HS.toList)
                  <&> HS.fromList

                path .= _path
                dateModified .= _dateModified
                masterField .= _masterField
                fields .= _fields) emptyEntry
          from _ = Nothing
