{-# LANGUAGE OverloadedStrings #-}

module Aoc2021.D1 where

import           Aoc2021.Util

import qualified Data.Text as Text
import           Data.Text ( Text )

d1t2 :: Text -> Int
d1t2 = count . map (uncurry (<)) . pairs . map sum . windows 3 . parse @Int
  where count = foldr (\a b -> if a then b+1 else b) 0

d1t1 :: Text -> Int
d1t1 = count . map (uncurry (<)) . pairs . parse @Int
  where count = foldr (\a b -> if a then b+1 else b) 0

-- | Parse each line into a numeric ordinal.
parse :: (Read a, Ord a, Num a) => Text -> [a]
parse = map tread . Text.lines

-- | Return all "windows" of a given size into a list. Like chunking, but
-- windows overlap and are always the same requested size i.e. 'windows n'
-- returns a list of @n@-tuples.
--
--  >>> windows 2 [1,2,3]
--  [[1,2],[2,3]]
windows :: Int -> [a] -> [[a]]
windows size = go
  where go xs = if length xs < size then [] else take size xs : go (tail xs)

-- | 'windows 2' shoved into an actual 2-tuple.
pairs :: [a] -> [(a, a)]
pairs = map (\[a, b] -> (a, b)) . windows 2
