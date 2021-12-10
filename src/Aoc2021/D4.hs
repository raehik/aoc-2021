{-# LANGUAGE OverloadedStrings #-}

module Aoc2021.D4 where

import           Aoc2021.Util
import           GHC.TypeNats
import           GHC.Generics
import qualified Data.List      as List
import           Optics
import           Data.Generics.Product.Any
import qualified Data.Text      as Text
import           Data.Text      ( Text )

data Board (n :: Natural) = Board { board :: [Cell] }
    deriving (Eq, Show, Generic)
data Cell = Cell { cellNum :: Natural, cellMarked :: Bool }
    deriving (Eq, Show, Generic)

t1 :: Text -> Natural
t1 x =
    let (ns, bs) = parse x
     in case runBingo bs ns of
          ([b], Just finalNum) -> boardScorePart b * finalNum
          _ -> error "unexpected failure in bingo subsystem"

t2 :: Text -> Natural
t2 x =
    let (ns, bs) = parse x
     in case runBingoGetLast bs ns of
          ([b], Just finalNum) -> boardScorePart b * finalNum
          _ -> error "unexpected failure in bingo subsystem"

parse :: Text -> ([Natural], [Board 5])
parse x =
    let parts = splitOn "" $ Text.lines x
        numsStr = head $ head parts
        nums = map tread (Text.splitOn "," numsStr)
        boards = map parseBoard $ tail parts
     in (nums, boards)

parseBoard :: [Text] -> Board n
parseBoard x =
    let x'    = map (map (tread @Natural)) $ map Text.words x
        cells = map (\n -> Cell n False) $ concat x'
     in Board cells

-- TODO make an alternative stateful version for t2
runBingo :: KnownNat n => [Board n] -> [Natural] -> ([Board n], Maybe Natural)
runBingo = go Nothing
  where
    go nPrev _  []     = ([], nPrev)
    go nPrev bs (n:ns) =
        case winningBoards bs of
          [] -> go (Just n) (map (tickNum n) bs) ns
          bs' -> (bs', nPrev)

runBingoGetLast :: KnownNat n => [Board n] -> [Natural] -> ([Board n], Maybe Natural)
runBingoGetLast = go [] Nothing
  where
    go wbs nPrev []  _  = (wbs, nPrev)
    go wbs nPrev _   [] = (wbs, nPrev)
    go _   _     bs  (n:ns) =
        let bs'   = map (tickNum n) bs
            wbs'  = filter boardIsWinning bs'
            bs''  = filter (not . boardIsWinning) bs'
         in go wbs' (Just n) bs'' ns

boardScorePart :: Board n -> Natural
boardScorePart = sum . map cellNum . filter (not . cellMarked) . board

tickNum :: Natural -> Board n -> Board n
tickNum n = over (the @"board") $ map $ \c -> if   cellNum c == n
                                              then c { cellMarked = True }
                                              else c

winningBoards :: KnownNat n => [Board n] -> [Board n]
winningBoards = filter boardIsWinning

boardIsWinning :: KnownNat n => Board n -> Bool
boardIsWinning b = or $ map (checkBingoLine b) $ bingoLines (natVal b)

-- @n < n^2@
boardCell :: Board n -> Natural -> Cell
boardCell b n = board b `List.genericIndex` n

-- Length is @2n + 2@.
bingoLines :: Natural -> [[Natural]]
bingoLines n = concat [bingoRows n, bingoCols n] -- , bingoDiags n]

-- Indices have to be @< n^2@.
checkBingoLine :: Board n -> [Natural] -> Bool
checkBingoLine b = and . map (cellMarked . boardCell b)

-- Length is @x@.
bingoRows :: Natural -> [[Natural]]
bingoRows x = go x
  where
    go 0 = []
    go n = [x*(n-1) .. x*n-1] : go (n-1)

-- Length is @x@.
bingoCols :: Natural -> [[Natural]]
bingoCols x = go x
  where
    go 0 = []
    go n = [n-1, n-1+x .. x*x-1] : go (n-1)

-- Length is @2@.
bingoDiags :: Natural -> [[Natural]]
bingoDiags 1 = [[1]]
bingoDiags x = [[0, x+1 .. x*x], [x-1, 2*(x-1) .. x*(x-1)]]

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)
