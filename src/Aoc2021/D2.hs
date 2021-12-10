{-# LANGUAGE OverloadedStrings #-}

module Aoc2021.D2 where

import           Aoc2021.Util

import qualified Data.Text as Text
import           Data.Text ( Text )
import           Control.Monad.State
import           Optics
import           Data.Generics.Product.Any
import           GHC.Generics

t2 :: Text -> Int
t2 t =
    let course = parse t
        subEndPos = execState (simulateCourse course) subInitial
     in subX subEndPos * subY subEndPos

data Sub = Sub
  { subX   :: Int
  , subY   :: Int
  , subAim :: Int
  } deriving (Eq, Show, Generic)

subInitial :: Sub
subInitial = Sub 0 0 0

simulateCourse :: [(Cmd, Int)] -> State Sub ()
simulateCourse = mapM_ go
  where
    go (c,n) = modify $
        case c of
          CmdForward ->
            \s -> s { subX = subX s + n, subY = subY s + (subAim s * n) }
          CmdDown    -> over (the @"subAim") (+n)
          CmdUp      -> over (the @"subAim") (flip (-) n)

t1 :: Text -> Int
t1 t =
    let course = parse t
        subEndPos = execState (simulateCourse' course) sub'Initial
     in sub'X subEndPos * sub'Y subEndPos

data Sub' = Sub'
  { sub'X :: Int
  , sub'Y :: Int
  } deriving (Eq, Show, Generic)

sub'Initial :: Sub'
sub'Initial = Sub' 0 0

simulateCourse' :: [(Cmd, Int)] -> State Sub' ()
simulateCourse' = mapM_ go
  where
    go (c,n) = modify $ case c of CmdForward -> over (the @"sub'X") (+n)
                                  CmdDown    -> over (the @"sub'Y") (+n)
                                  CmdUp      -> over (the @"sub'Y") (flip (-) n)


data Cmd
  = CmdForward
  | CmdDown
  | CmdUp

parse :: Text -> [(Cmd, Int)]
parse = map parseCmd . Text.lines

parseCmd :: Text -> (Cmd, Int)
parseCmd = (\[c, n] -> (parseCmd' c, tread n)) . Text.words

parseCmd' :: Text -> Cmd
parseCmd' = \case "forward" -> CmdForward
                  "down"    -> CmdDown
                  "up"      -> CmdUp
                  _         -> error "invalid command"
