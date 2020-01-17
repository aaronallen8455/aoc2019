{-# LANGUAGE OverloadedStrings #-}
module Day.Three
  ( dayThreeA
  , dayThreeB
  ) where

import           Control.Monad (guard)
import qualified Data.ByteString.Char8 as BS
import           Data.Bifunctor (bimap)
import qualified Data.IntMap as M
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)

import           Day.Common (commaSep, readInt)

data Wire =
  Wire { colRanges :: M.IntMap [((Int, Int), Int)]
       , rowRanges :: M.IntMap [((Int, Int), Int)]
       } deriving Show

data Direction
  = V Int
  | H Int

parseDir :: BS.ByteString -> Maybe Direction
parseDir bs = case BS.uncons bs of
  Just ('U', r) -> V <$> readInt r
  Just ('D', r) -> V . negate <$> readInt r
  Just ('L', r) -> H . negate <$> readInt r
  Just ('R', r) -> H <$> readInt r
  _             -> Nothing

dayThreeA :: BS.ByteString -> BS.ByteString
dayThreeA inp = fromMaybe "bad input" $ do
  [one, two] <- (traverse . traverse) parseDir $ commaSep <$> BS.lines inp
  let wire = mkWire two
      xs = intersections wire one
      closest = minimum . dropOrigin $ map (uncurry (+) . bimap abs abs) xs
  pure . BS.pack $ show closest

dayThreeB :: BS.ByteString -> BS.ByteString
dayThreeB inp = fromMaybe "bad input" $ do
  [one, two] <- (traverse . traverse) parseDir $ commaSep <$> BS.lines inp
  let wire = mkWire two
      dists = distances wire one
      closest = minimum $ filter (>0) dists
  pure . BS.pack $ show closest

mkWire :: [Direction] -> Wire
mkWire = go (0, 0) 0 (Wire mempty mempty) where
  go :: (Int, Int) -> Int -> Wire -> [Direction] -> Wire
  go (cCol, cRow) dist (Wire cols rows) (d : ds) =
    case d of
      V n -> go (cCol, cRow + n) (dist + abs n) (Wire cols $ M.insertWith (++) cCol [((cRow, cRow + n), dist + abs n)] rows) ds
      H n -> go (cCol + n, cRow) (dist + abs n) (Wire (M.insertWith (++) cRow [((cCol, cCol + n), dist + abs n)] cols) rows) ds
  go _ _ wire [] = wire

distances :: Wire -> [Direction] -> [Int]
distances (Wire cols rows) = snd . foldl' go (((0, 0), 0), []) where
  go (((cCol, cRow), dist), acc) d =
    case d of
      V n -> ( ((cCol, cRow + n), dist + abs n)
             , [ a + b
               | ((x, _), b) <- overlappingRanges cCol (sortTuple (cRow, cRow + n)) rows
                             ++ crosses cCol (sortTuple (cRow, cRow + n)) cols
               , Just a <- [crossDistance x ((cRow, cRow + n), dist + abs n)]
               ] ++ acc
             )
      H n -> ( ((cCol + n, cRow), dist + abs n)
             , [ a + b
               | ((x, _), b) <- overlappingRanges cRow (sortTuple (cCol, cCol + n)) cols
                             ++ crosses cRow (sortTuple (cCol, cCol + n)) rows
               , Just a <- [crossDistance x ((cCol, cCol + n), dist + abs n)]
               ] ++ acc
             )

intersections :: Wire -> [Direction] -> [(Int, Int)]
intersections (Wire cols rows) = snd . foldl' go ((0, 0), []) where
  go ((cCol, cRow), xs) d =
    case d of
      V n ->
        let c = overlappingRanges cCol (sortTuple (cRow, cRow + n)) rows
             ++ crosses cCol (sortTuple (cRow, cRow + n)) cols
         in ((cCol, cRow + n), map fst c ++ xs)
      H n ->
        let c = overlappingRanges cRow (sortTuple (cCol, cCol + n)) cols
             ++ crosses cRow (sortTuple (cCol, cCol + n)) rows
         in ((cCol + n, cRow), map fst c ++ xs)

overlappingRanges :: Int -> (Int, Int) -> M.IntMap [((Int, Int), Int)] -> [((Int, Int), Int)]
overlappingRanges i r m =
  [ ((a, i), d)
  | (range, dist) <- concat $ M.lookup i m
  , a <- uncurry enumFromTo range
  , inRange a sortedR
  , Just d <- [crossDistance a (range, dist)]
  ]
  where
    sortedR = sortTuple r

crosses :: Int -> (Int, Int) -> M.IntMap [((Int, Int), Int)] -> [((Int, Int), Int)]
crosses i r m =
  [ ((a, i), d)
  | (a, (range, dist)) <- findInRange sortedR m
  , Just d <- [crossDistance i (range, dist)]
  ]
  where
    sortedR = sortTuple r

-- first arg is the cross index that we are looking for in the range
-- second arg is the range along with total distance
crossDistance :: Int -> ((Int, Int), Int) -> Maybe Int
crossDistance i ((a, b), dist)
  | inRange i (sortTuple (a, b)) = Just $ dist - (abs $ b - i)
  | otherwise = Nothing

sortTuple :: (Int, Int) -> (Int, Int)
sortTuple (a, b) = (min a b, max a b)

findInRange :: (Int, Int) -> M.IntMap [((Int, Int), Int)] -> [(Int, ((Int, Int), Int))]
findInRange (start, end) m = concat $ do
  (end', a) <- M.lookupLE end m
  guard $ end' >= start
  pure $ (zip (repeat end') $ a) ++ findInRange (start, (end' - 1)) m

inRange :: Int -> (Int, Int) -> Bool
inRange i (s, e) = i >= s && i <= e

dropOrigin :: [Int] -> [Int]
dropOrigin [] = []
dropOrigin (0:xs) = xs
dropOrigin (x:xs) = x : dropOrigin xs
