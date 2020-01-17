{-# LANGUAGE OverloadedStrings #-}
module Day.Seventeen
  ( daySeventeenA
  , daySeventeenB
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IM
import           Data.Maybe (catMaybes, fromMaybe)

import           Day.IntCode (parseInput, runIntCodeProgram)

daySeventeenA :: BS8.ByteString -> BS8.ByteString
daySeventeenA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  output <- traverse parseTile =<< runIntCodeProgram [] codes
  let result = sum $ findParameters 0 (repeat E) output
  pure . BS8.pack $ show result

daySeventeenB :: BS8.ByteString -> BS8.ByteString
daySeventeenB inp = fromMaybe "invalid program" $ do
  codes <- IM.insert 0 2 <$> parseInput inp
  let main = fromEnum <$> "A,B,B,A,C,A,A,C,B,C\n"
      a = fromEnum <$> "R,8,L,12,R,8\n"
      b = fromEnum <$> "R,12,L,8,R,10\n"
      c = fromEnum <$> "R,8,L,8,L,8,R,8,R,10\n"
      n = fromEnum <$> "n\n"
  result : _ <- reverse <$> runIntCodeProgram (concat [main, a, b , c, n]) codes
  pure . BS8.pack $ show result

findParameters :: Int -> [Tile] -> [Tile] -> [Int]
findParameters _ _ [] = []
findParameters rowIx lastRow xs = catMaybes params ++ findParameters (rowIx + 1) (E : row') rest
  where
  (row, rest) = break (== NewLine) $ dropWhile (== NewLine) xs
  (row', params) = unzip $ track lastRow (zip [0..] row)
  track :: [Tile] -> [(Int, Tile)] -> [(Tile, Maybe Int)]
  track (_ : r1@(S _) : r2 : rs)
        ((_, S _) : l1@(_, S _) : l2@(_, S _) : ls)
    = (S Cross, Nothing) : track (r1 : r2 : rs) (l1 : l2 : ls)
  track (_ : r1@(S Cross) : r2 : rs)
        (_ : l1@(colIx, S _) : l2 : ls)
    = (snd l1, Just $ colIx * (rowIx - 1)) : track (r1 : r2 : rs) (l1 : l2 : ls)
  track (_ : r1 : r2 : rs)
        (_ : l1 : l2 : ls)
    = (snd l1, Nothing) : track (r1 : r2 : rs) (l1 : l2 : ls)
  track _ [(_, a), (_, b)] = [(a, Nothing), (b, Nothing)]
  track _ _ = []

data Tile
  = E -- empty
  | S Scaffold
  | NewLine
  deriving (Eq)

instance Show Tile where
  show E = "."
  show (S Plain) = "#"
  show (S Cross) = "O"
  show (S (B U)) = "^"
  show (S (B R)) = ">"
  show (S (B L)) = "<"
  show (S (B D)) = "v"
  show NewLine = ""

data Scaffold
  = Plain
  | Cross
  | B Dir -- robot
  deriving (Show, Eq)

data Dir
  = U
  | R
  | D
  | L
  deriving (Show, Eq)

parseTile :: Int -> Maybe Tile
parseTile i =
  case i of
    10 -> Just NewLine
    35 -> Just $ S Plain
    46 -> Just E
    60 -> Just . S $ B L
    62 -> Just . S $ B R
    94 -> Just . S $ B U
    118 -> Just . S $ B D
    _ -> Nothing

