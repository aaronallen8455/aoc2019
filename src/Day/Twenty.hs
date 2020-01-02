{-# LANGUAGE OverloadedStrings #-}
module Day.Twenty where

import           Data.Array
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isAlpha)
import           Data.List (transpose)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Tuple (swap)

dayTwentyA :: BS8.ByteString -> BS8.ByteString
dayTwentyA inp = fromMaybe "invalid input" $ do
  let rows = BS8.unpack <$> BS8.lines inp
      portals = parsePortals rows
  maze <- parseMaze rows portals
  pure "test"

parseMaze :: [String] -> [((Int, Int), Tile)] -> Maybe Maze
parseMaze rows portals = do
    h : _ <- Just rows
    let width = length h
        height = length rows
    pure . accumArray f Empty ((0, 0), (width - 1, height - 1)) . (portals <>) $
      concat [ [ ((y, x), toTile c)
               | (x, c) <- zip [0..] r
               ]
             | (y, r) <- zip [0..] rows
             ]
  where
    toTile '.' = Empty
    toTile _ = Wall
    f Empty x = x
    f x Empty = x

parsePortals :: [String] -> [((Int, Int), Tile)]
parsePortals rows = toTile $ rPortals <> cPortals
  where
    portal x y (a : b : '.' : rest)
      | isAlpha a && isAlpha b
      = ((x + 2, y), [a, b]) : portal (x + 3) y rest
    portal x y ('.' : a : b : rest)
      | isAlpha a && isAlpha b
      = ((x, y), [a, b]) : portal (x + 3) y rest
    portal x y (_ : a : b : rest) = portal (x + 1) y (a : b : rest)
    port _ _ _ = []
    rPortals = concatMap (\(y, r) -> portal 0 y r) (zip [0..] rows)
    cPortals = first swap
           <$> concatMap (\(y, r) -> portal 0 y r) (zip [0..] $ transpose rows)
    toTile ((c, "AA") : rest) = (c, Start) : toTile rest
    toTile ((c, "ZZ") : rest) = (c, End) : toTile rest
    toTile ((c1, p) : rest)
      | (b, (c2, _) : r) <- break ((== p) . snd) rest
      = (c1, Portal c2) : (c2, Portal c1) : toTile (b ++ r)
      | otherwise = toTile rest

type Maze = Array (Int, Int) Tile

data Tile
  = Empty
  | Wall
  | Start
  | End
  | Portal (Int, Int)
