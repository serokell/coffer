-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.ParseError
  ( ParseError (..)
  , line
  , offset
  , errorMessage
  , parseParseError
  ) where

import CLI.Parser (MParser)
import Control.Lens
import Control.Monad (void)
import Data.Text (Text)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

data ParseError = ParseError
  { peLine :: Int
  , peOffset :: Int
  , peErrorMessage :: Text
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''ParseError

spaceConsumer :: MParser ()
spaceConsumer = L.space P.space1 P.empty P.empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> MParser Text
symbol = L.symbol spaceConsumer

parseInt :: MParser Int
parseInt = lexeme L.decimal

parseParseError :: MParser ParseError
parseParseError = do
  line <- parseInt
  void $ symbol ":"
  offset <- parseInt
  P.manyTill (P.satisfy $ const True) (P.lookAhead $ P.string "unexpected")
  ParseError line offset <$> P.takeRest
