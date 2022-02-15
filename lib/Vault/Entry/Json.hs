{-# LANGUAGE LambdaCase #-}
module Vault.Entry.Json where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import Vault.Entry
import qualified Vault.Entry as E
import Control.Lens
import qualified Data.HashMap.Strict as HS
import Control.Monad.State (execState, runState, execStateT, lift)
import qualified Data.Text as T
import Control.Applicative (liftA2)
import qualified Control.Monad.Writer.Strict as T

newtype JsonEntry = JsonEntry A.Value

fieldConverter :: (Lens Field (Maybe Field) A.Value A.Value)
fieldConverter = lens getter setter
  where getter field =
          A.object
          [ "date_modified" A..= (field ^. dateModified)
          , "value" A..= (field ^. value)
          ]
        setter e (A.Object o) =
          (execStateT $
            do
              _dateModified <- lift $ HS.lookup "date_modified" o
                >>= \case A.String t -> Just t ; _ -> Nothing
              _value <- lift $ HS.lookup "value" o
                >>= \case A.String t -> Just t ; _ -> Nothing

              dateModified .= _dateModified
              value .= _value) e
        setter _ _ = Nothing

instance EntryConvertible JsonEntry where
  entry = lens getter setter
    where getter entry =
            JsonEntry $ A.object
            [ "path" A..= (entry ^. path)
            , "date_modified" A..= (entry ^. dateModified)
            , "fields" A..= HS.map (\b -> let un = getter2 b in undefined :: A.Value) (entry ^. fields)
            ]
          getter2 field =
            A.object
            [ "date_modified" A..= (field ^. dateModified)
            , "value" A..= (field ^. value)
            ]
          setter e (JsonEntry (A.Object o)) =
            (execStateT $
              do
                _path <- lift $ HS.lookup "path" o
                  >>= (\case A.String t -> Just t ; _ -> Nothing)
                  <&> T.split (== ' ')
                _dateModified <- lift $ HS.lookup "date_modified" o
                  >>= \case A.String t -> Just t ; _ -> Nothing
                _fields <- lift $ HS.lookup "fields" o
                  >>= (\case A.Object a -> Just a ; _ -> Nothing)
                  <&> HS.map (\v -> set fieldConverter v emptyField)
                  >>= (mapM (uncurry (liftA2 (,)) . over _1 newFieldKey) . HS.toList)
                  <&> HS.fromList

                path .= _path
                dateModified .= _dateModified
                fields .= _fields) e
          setter _ _ = Nothing
