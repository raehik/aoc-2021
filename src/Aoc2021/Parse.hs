module Aoc2021.Parse
  ( module Aoc2021.Parse
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
  , module Control.Monad.Combinators
  ) where

import Control.Monad.Combinators
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding ( space )

import qualified Data.Text          as Text
import           Data.Text          ( Text )
import           Data.Void
import qualified Text.Megaparsec    as Megaparsec
import           Text.Megaparsec    ( Parsec )
import           Data.Maybe         ( fromJust )

type Parser = Parsec Void Text

tread :: Read a => Text -> a
tread = read . Text.unpack

parseMega :: Parser a -> Text -> a
parseMega p = fromJust . Megaparsec.parseMaybe p
