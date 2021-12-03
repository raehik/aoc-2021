{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Aoc2021.D3 where

import qualified Data.Text as Text
import           Data.Text ( Text )

t1 :: Int -> Text -> Int
t1 bits t =
    let parsed = parse t
        bitOccurences = map (flip extractBitPosition parsed) [0..bits-1]
        gamma = map isMoreTrueThanFalse bitOccurences
        epsilon = map not gamma
        pwrConsumption = bitVecToBE gamma * bitVecToBE epsilon
     in pwrConsumption

t2 :: Text -> Int
t2 t = bitVecToBE (oxy p) * bitVecToBE (co2 p)
  where p = parse t

compareBitCounts :: [Bool] -> Ordering
compareBitCounts = uncurry compare . foldr go (0, 0)
  where
    go :: Bool -> (Integer, Integer) -> (Integer, Integer)
    go True  (ones, zeroes) = (ones+1, zeroes)
    go False (ones, zeroes) = (ones, zeroes+1)

oxy :: [[Bool]] -> [Bool]
oxy = go 0
  where
    go _   [x] = x
    go bit xs  =
        let bits = extractBitPosition bit xs
            xs'  = case compareBitCounts bits of
                     LT -> filter (\x' -> not (x' !! bit)) xs
                     _  -> filter (!! bit) xs
         in go (bit+1) xs'

co2 :: [[Bool]] -> [Bool]
co2 = go 0
  where
    go _   [x] = x
    go bit xs  =
        let bits = extractBitPosition bit xs
            xs'  = case compareBitCounts bits of
                     LT -> filter (!! bit) xs
                     _  -> filter (\x' -> not (x' !! bit)) xs
         in go (bit+1) xs'

isMoreTrueThanFalse :: [Bool] -> Bool
isMoreTrueThanFalse x =
    case compareBitCounts x of
      GT -> True
      EQ -> error "identical trues & falses"
      LT -> False

parse :: Text -> [[Bool]]
parse = map parseLine . Text.lines
  where
    parseLine = Text.foldr (\ch bs -> parseBit ch : bs) []
    parseBit = \case '0' -> False; '1' -> True; _ -> error "parse error"

extractBitPosition :: Int -> [[Bool]] -> [Bool]
extractBitPosition bitPos = foldr (\x xs -> (!! bitPos) x : xs) []

bitVecToBE :: Integral a => [Bool] -> a
bitVecToBE = fst . foldr (\b (x, n) -> (go x n b, n+1)) (0, 0)
  where
    go :: Integral a => a -> a -> Bool -> a
    go x n = \case False -> x; True -> x + 2^n

exIn :: Text
exIn = Text.unlines [ "00100"
                    , "11110"
                    , "10110"
                    , "10111"
                    , "10101"
                    , "01111"
                    , "00111"
                    , "11100"
                    , "10000"
                    , "11001"
                    , "00010"
                    , "01010" ]
