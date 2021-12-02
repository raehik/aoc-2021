module Aoc2021.Util where

import qualified Data.Text as Text
import           Data.Text ( Text )

tread :: Read a => Text -> a
tread = read . Text.unpack
