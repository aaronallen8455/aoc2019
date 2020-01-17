{-# LANGUAGE OverloadedStrings #-}
module Day.Twenty where

import           Control.Applicative ((<|>))
import           Control.Arrow ((&&&))
import           Control.Monad (foldM)
import           Data.Array
import           Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isAlpha)
import           Data.List (find, foldl', transpose)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Tuple (swap)

dayTwentyA :: BS8.ByteString -> BS8.ByteString
dayTwentyA inp = fromMaybe "invalid input" $ do
  let rows = BS8.unpack <$> BS8.lines inp
      portals = parsePortals rows
  maze <- parseMaze rows portals
  ((start, _), (end, _))
    <- bitraverse id id
     . (find ((== Start) . snd) &&& find ((== End) . snd))
     $ assocs maze
  res <- shortestPath maze [start] [end] S.empty M.empty 0
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
  res <- shortestPathB maze [(start, 0)] [(end, 0)] S.empty M.empty 0
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

shortestPath :: Maze -> [(Int, Int)] -> [(Int, Int)] -> S.Set (Int, Int) -> M.Map (Int, Int) Int -> Int -> Maybe Int
shortestPath _ [] _ _ _ _ = Nothing
shortestPath _ _ [] _ _ _ = Nothing
shortestPath maze sq eq sv ev acc =
  let stepStart (visited, qAcc) coord@(x, y) =
        case M.lookup coord ev of
          Just d -> Left $ acc + d
          Nothing ->
            let moves = [ c
                        | Portal c _ <- [maze ! coord]
                        , not $ S.member c visited
                        ]
                        <|>
                        [ c
                        | c <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                        , maze ! c /= Wall
                        , not $ S.member c visited
                        ]
             in Right (foldr S.insert visited moves, foldr (:) qAcc moves)
      stepEnd (visited, qAcc) coord@(x, y) =
        let moves = [ (c, acc + 1)
                    | Portal c _ <- [maze ! coord]
                    , not $ M.member c visited
                    ]
                    <|>
                    [ (c, acc + 1)
                    | c <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                    , maze ! c /= Wall
                    , not $ M.member c visited
                    ]
         in (foldr (uncurry M.insert) visited moves, foldr (:) qAcc (fst <$> moves))
   in case foldM stepStart (sv, []) sq of
        Left r -> Just r
        Right (sv', sq') ->
          let (ev', eq') = foldl' stepEnd (ev, []) eq
           in shortestPath maze sq' eq' sv' ev' $! acc + 1

shortestPathB :: Maze -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> S.Set ((Int, Int), Int) -> M.Map ((Int, Int), Int) Int -> Int -> Maybe Int
shortestPathB _ [] _ _ _ _ = Nothing
shortestPathB _ _ [] _ _ _ = Nothing
shortestPathB maze sq eq sv ev acc =
  let stepStart (visited, qAcc) pos@(coord@(x, y), lvl) =
        case M.lookup pos ev of
          Just d -> Left $ acc + d
          Nothing ->
            let moves = [ (c, lvl')
                        | Portal c outer <- [maze ! coord]
                        , not $ lvl == 0 && outer
                        , let lvl' = if outer
                                        then lvl - 1
                                        else lvl + 1
                        , not $ S.member (c, lvl') visited
                        ]
                        <|>
                        [ (c, lvl)
                        | c <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                        , maze ! c /= Wall
                        , not $ S.member (c, lvl) visited
                        ]
             in Right (foldr S.insert visited moves, foldr (:) qAcc moves)
      stepEnd (visited, qAcc) (coord@(x, y), lvl) =
        let moves = [ ((c, lvl'), acc + 1)
                    | Portal c outer <- [maze ! coord]
                    , not $ lvl == 0 && outer
                    , let lvl' = if outer
                                    then lvl - 1
                                    else lvl + 1
                    , not $ M.member (c, lvl') visited
                    ]
                    <|>
                    [ ((c, lvl), acc + 1)
                    | c <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                    , maze ! c /= Wall
                    , not $ M.member (c, lvl) visited
                    ]
         in (foldr (uncurry M.insert) visited moves, foldr (:) qAcc (fst <$> moves))
   in case foldM stepStart (sv, []) sq of
        Left r -> Just r
        Right (sv', sq') ->
          let (ev', eq') = foldl' stepEnd (ev, []) eq
           in shortestPathB maze sq' eq' sv' ev' $! acc + 1
