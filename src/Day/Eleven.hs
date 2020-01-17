{-# LANGUAGE OverloadedStrings #-}
module Day.Eleven
  ( dayElevenA
  , dayElevenB
  ) where

import           Control.Monad.Fix
import           Control.Monad.Trans.State
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import           Data.Function (on)
import           Data.List (groupBy)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Tuple (swap)

import           Day.IntCode (parseInput, runIntCodeProgram)

-- why does it work with State but not StateT?
dayElevenA :: BS8.ByteString -> BS8.ByteString
dayElevenA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  let (_, (m, _, _)) = (`runState` (M.empty, N, (0,0))) $
        mfix $ \mbOut ->
          flip runIntCodeProgram codes
                  <$> fmap (0 :) (handleOutput $ concat mbOut)
  pure $ BS8.pack . show $ M.size m

dayElevenB :: BS8.ByteString -> BS8.ByteString
dayElevenB inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  let (_, (m, _, _)) = (`runState` (M.empty, N, (0,0))) $
        mfix $ \mbOut ->
          flip runIntCodeProgram codes
                  <$> fmap (1 :) (handleOutput $ concat mbOut)
      coords = map fst . filter snd $ M.toList m
      (colMin, colMax) = (fst (head coords), fst (last coords))
      (rowMin, rowMax) = let rows = snd <$> coords in (minimum rows, maximum rows)
      arr = toPaint <$> accumArray (||) False ((rowMin, colMin), (rowMax, colMax)) (map swap coords `zip` repeat True)
  pure . BS8.unlines . reverse $
    [ BS8.pack $ snd <$> row
    | row <- groupBy ((==) `on` fst . fst) $ assocs arr
    ]

toPaint :: Bool -> Char
toPaint True = '#'
toPaint False = '.'

handleOutput :: [Int] -> State (M.Map (Int,Int) Bool, Direction, (Int, Int)) [Int]
handleOutput (a : b : xs) = do
  (m, dir, coords) <- get
  let paint = toEnum a
      t = toEnum b
      m' = M.insert coords paint m
      dir' = turn dir t
      coords' = move dir' coords
      curPaint = fromMaybe False $ M.lookup coords' m
  put $ (m', dir', coords')
  (fromEnum curPaint :) <$> handleOutput xs
handleOutput _ = pure []

data Direction
  = N
  | E
  | S
  | W

data Turn
  = L
  | R
  deriving Enum

turn :: Direction -> Turn -> Direction
turn N L = W
turn N R = E
turn E L = N
turn E R = S
turn S L = E
turn S R = W
turn W L = S
turn W R = N

move :: Direction -> (Int, Int) -> (Int, Int)
move N (x, y) = (x, y + 1)
move E (x, y) = (x + 1, y)
move S (x, y) = (x, y - 1)
move W (x, y) = (x - 1, y)

