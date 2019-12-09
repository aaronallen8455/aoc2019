module Day.Three
  ( dayThreeA
  ) where

import           Control.Monad (guard)
import           Data.Bifunctor (bimap)
import           Data.Function (on)
import qualified Data.IntMap as M
import           Data.List (foldl', groupBy)
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Tuple (swap)
import           Text.Read (readMaybe)

import           Day.Common (commaSep)

data Wire =
  Wire { colRanges :: M.IntMap [(Int, Int)]
       , rowRanges :: M.IntMap [(Int, Int)]
       } deriving Show

data Direction
  = V Int
  | H Int

parseDir :: String -> Maybe Direction
parseDir ('U':r) = V <$> readMaybe r
parseDir ('D':r) = V . negate <$> readMaybe r
parseDir ('L':r) = H . negate <$> readMaybe r
parseDir ('R':r) = H <$> readMaybe r
parseDir _ = Nothing

dayThreeA :: String -> String
dayThreeA inp = fromMaybe "bad input" $ do
  [one, two] <- (traverse . traverse) parseDir $ commaSep <$> lines inp
  let wire = mkWire two
      xs = intersections wire one
      closest = minimum . dropOrigin $ map (uncurry (+) . bimap abs abs) xs
  pure $ show closest

mkWire :: [Direction] -> Wire
mkWire = go (0, 0) (Wire mempty mempty) where
  go :: (Int, Int) -> Wire -> [Direction] -> Wire
  go (cCol, cRow) (Wire cols rows) (d : ds) =
    case d of
      V n -> go (cCol, cRow + n) (Wire cols $ M.insertWith (++) cCol [ordTuple cRow (cRow + n)] rows) ds
      H n -> go (cCol + n, cRow) (Wire (M.insertWith (++) cRow [ordTuple cCol (cCol + n)] cols) rows) ds
  go _ wire [] = wire

intersections :: Wire -> [Direction] -> [(Int, Int)]
intersections (Wire cols rows) = snd . foldl' go ((0, 0), []) where
  go ((cCol, cRow), xs) d =
    case d of
      V n ->
        let c = overlappingRanges cCol (ordTuple cRow $ cRow + n) rows
             ++ crosses cCol (ordTuple cRow $ cRow + n) cols
         in ((cCol, cRow + n), c ++ xs)
      H n ->
        let c = overlappingRanges cRow (ordTuple cCol $ cCol + n) cols
             ++ crosses cRow (ordTuple cCol $ cCol + n) rows
         in ((cCol + n, cRow), c ++ xs)

overlappingRanges :: Int -> (Int, Int) -> M.IntMap [(Int, Int)] -> [(Int, Int)]
overlappingRanges i r m
  = concatMap (enumRange i) . concat
  $ catMaybes . map (rangeIntersect r) <$> M.lookup i m

crosses :: Int -> (Int, Int) -> M.IntMap [(Int, Int)] -> [(Int, Int)]
crosses i r = map (bimap id (const i)) . filter (inRange i . snd) . findInRange r

ordTuple :: Int -> Int -> (Int, Int)
ordTuple a b = (min a b, max a b)

findInRange :: (Int, Int) -> M.IntMap [(Int, Int)] -> [(Int, (Int, Int))]
findInRange (start, end) m = concat $ do
  (end', a) <- M.lookupLE end m
  guard $ end' >= start
  pure $ (zip (repeat end') a) ++ findInRange (start, (end' - 1)) m

inRange :: Int -> (Int, Int) -> Bool
inRange i (s, e) = i >= s && i <= e

rangeIntersect :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
rangeIntersect r1@(as, ae) r2@(bs, be)
  | inRange as r2 = Just (as, min ae be)
  | inRange bs r1 = Just (bs, min ae be)
  | otherwise = Nothing

enumRange :: Int -> (Int, Int) -> [(Int,Int)]
enumRange a (b, c) = zip (repeat a) [b..c]

dropOrigin :: [Int] -> [Int]
dropOrigin [] = []
dropOrigin (0:xs) = xs
dropOrigin (x:xs) = x : dropOrigin xs
