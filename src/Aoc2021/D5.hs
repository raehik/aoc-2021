{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Aoc2021.D5 where

import qualified Aoc2021.Parse  as P
import           Aoc2021.Parse  ( Parser )
import           Data.Text      ( Text )

data Coord = Coord { coordX :: Integer, coordY :: Integer } deriving (Eq, Show)

parse :: Text -> [(Coord, Coord)]
parse = P.parseMega pData

pData :: Parser [(Coord, Coord)]
pData = P.sepBy pCoordRange P.newline

pCoordRange :: Parser (Coord, Coord)
pCoordRange = do
    c1 <- pCoord
    _  <- P.string " -> "
    c2 <- pCoord
    return $ (c1, c2)

pCoord :: Parser Coord
pCoord = do
    coordX <- P.decimal
    _      <- P.char ','
    coordY <- P.decimal
    return $ Coord{..}

-- _ :: [(Coord, Coord)] -> [Coord]
f :: (Coord, Coord) -> [Coord]
f (Coord c1x c1y) (Coord c2x c2y) = abs (c1y - c2y) n c1y c2y
