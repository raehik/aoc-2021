module Aoc2021.D4TypeLevel where

import           GHC.TypeNats
import           GHC.Exts
import           Data.Vector.Sized
import qualified Data.Vector.Sized as V
import           Finite

import           GHC.Generics
import qualified Data.Text as Text
import           Data.Text ( Text )

data Board (n :: Natural) = Board { board :: Vector (n * n) Cell }
    deriving (Eq, Ord, Show)
data Cell = Cell { cellNum :: Natural, cellMarked :: Bool }
    deriving (Eq, Ord, Show)

runBingo :: KnownNat n => [Board n] -> [Natural] -> ([Board n], Maybe Natural)
runBingo bs ns = ([], Nothing)

winningBoards :: [Board n] -> [Board n]
winningBoards = filter boardIsWinning

boardIsWinning :: Board n -> Bool
boardIsWinning = const False

boardCell :: KnownNat (n * n) => Board n -> Finite (n * n) -> Cell
boardCell (Board b) = V.index b . convertFinite

-- | Cool! But sadly, the value part is much too fiddly for me to remain
--   interested. I have to do various operations on naturals, and somehow retain
--   the proof that we're in indexing range. Everything else works well enough,
--   the type-level guarantees and such. But GHC offers no help with proofs.
--   This would be a decent way to get into "useful"/"real" type-level
--   programming, but hey, I've got an Advent to Code.
bingoLines
    :: forall n. (KnownNat n, KnownNat (n * n))
    => Vector (n + n + 2) (Vector n (Finite (n * n)))
bingoLines = rows V.++ columns V.++ diags
  where
    rows :: Vector n (Vector n (Finite (n * n)))
    --rows = V.generate $ \n -> V.replicate $ convertFinite' n
    rows = V.replicate $ V.replicate $ finite 0
    columns :: Vector n (Vector n (Finite (n * n)))
    columns = rows
    diags :: Vector 2 (Vector n (Finite (n * n)))
    diags = V.replicate $ V.replicate $ finite 0

checkBingoLine :: KnownNat (n * n) => Vector n (Finite (n * n)) -> Board n -> Bool
checkBingoLine v b = V.all (cellMarked . boardCell b) v

exBoard :: Board 3
exBoard = Board $ V.replicate (Cell 0 False)
