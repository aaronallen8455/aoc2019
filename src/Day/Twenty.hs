{-# LANGUAGE OverloadedStrings #-}
module Day.Twenty where

import           Control.Applicative ((<|>))
import           Control.Arrow ((&&&))
import           Data.Array
import           Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isAlpha)
import           Data.List (find, transpose)
import           Data.Maybe (fromMaybe)
import           Data.Tuple (swap)

import           Day.Common (biDirectionalBFS)

dayTwentyA :: BS8.ByteString -> BS8.ByteString
dayTwentyA inp = fromMaybe "invalid input" $ do
  let rows = BS8.unpack <$> BS8.lines inp
      portals = parsePortals rows
  maze <- parseMaze rows portals
  ((start, _), (end, _))
    <- bitraverse id id
     . (find ((== Start) . snd) &&& find ((== End) . snd))
     $ assocs maze
  res <- shortestPath maze start end
  pure . BS8.pack $ show res

dayTwentyB :: BS8.ByteString -> BS8.ByteString
dayTwentyB inp = fromMaybe "invalid input" $ do
  let rows = BS8.unpack <$> BS8.lines inp
      portals = parsePortals rows
  maze <- parseMaze rows portals
  ((start, _), (end, _))
    <- bitraverse id id
     . (find ((== Start) . snd) &&& find ((== End) . snd))
     $ assocs maze
  res <- shortestPathB maze start end
  pure . BS8.pack $ show res

parseMaze :: [String] -> [((Int, Int), Tile)] -> Maybe Maze
parseMaze rows portals = do
    h : _ <- Just rows
    let width = length h
        height = length rows
    pure . accumArray f Empty ((0, 0), (width - 1, height - 1)) . (portals <>) $
      concat [ [ ((x, y), toTile c)
               | (x, c) <- zip [0..] r
               ]
             | (y, r) <- zip [0..] rows
             ]
  where
    toTile '.' = Empty
    toTile _ = Wall
    f Empty x = x
    f x Empty = x
    f x _ = x

parsePortals :: [String] -> [((Int, Int), Tile)]
parsePortals rows = toTile $ rPortals <> cPortals
  where
    portal x y (a : b : '.' : rest)
      | isAlpha a && isAlpha b
      = ((x + 2, y), [a, b], x == 0) : portal (x + 3) y rest
    portal x y ('.' : a : b : rest)
      | isAlpha a && isAlpha b
      = ((x, y), [a, b], rest == []) : portal (x + 3) y rest
    portal x y (_ : a : b : rest) = portal (x + 1) y (a : b : rest)
    portal _ _ _ = []
    rPortals = concatMap (\(y, r) -> portal 0 y r) (zip [0..] rows)
    cPortals = (\(x, y, z) -> (swap x, y ,z))
           <$> concatMap (\(y, r) -> portal 0 y r) (zip [0..] $ transpose rows)
    toTile ((c, "AA", _) : rest) = (c, Start) : toTile rest
    toTile ((c, "ZZ", _) : rest) = (c, End) : toTile rest
    toTile ((c1, p, outer1) : rest)
      | (b, (c2, _, outer2) : r) <- break (\(_, x, _) -> x == p) rest
      = (c1, Portal c2 outer1) : (c2, Portal c1 outer2) : toTile (b ++ r)
      | otherwise = toTile rest
    toTile [] = []

type Maze = Array (Int, Int) Tile

data Tile
  = Empty
  | Wall
  | Start
  | End
  | Portal (Int, Int) Bool
  deriving (Eq, Show)

shortestPath :: Maze -> (Int, Int) -> (Int, Int) -> Maybe Int
shortestPath maze start end =
  fst <$> biDirectionalBFS next (start, ()) (end, ())
  where
    next coord@(x, y) _ =
      [ (c, ())
      | Portal c _ <- [maze ! coord]
      ]
      <|>
      [ (c, ())
      | c <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      , maze ! c /= Wall
      ]

shortestPathB :: Maze -> (Int, Int) -> (Int, Int) -> Maybe Int
shortestPathB maze start end =
  fst <$> biDirectionalBFS next ((start, 0 :: Int), ()) ((end, 0), ())
  where
    next (coord@(x, y), lvl) _ =
      [ ((c, lvl'), ())
      | Portal c outer <- [maze ! coord]
      , not $ lvl == 0 && outer
      , let lvl' = if outer
                      then lvl - 1
                      else lvl + 1
      ]
      <|>
      [ ((c, lvl), ())
      | c <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
      , maze ! c /= Wall
      ]

