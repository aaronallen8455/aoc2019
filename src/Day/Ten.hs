{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Day.Ten
  ( dayTenA
  , dayTenB
  ) where

import           Control.Monad (join)
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import           Data.Function (on)
import qualified Data.Map.Lazy as M
import           Data.List (transpose)
import qualified Data.Set as S
import qualified Data.Vector as V

dayTenA :: BS8.ByteString -> BS8.ByteString
dayTenA inp =
  let asteroids = V.fromList
                . map ((,) <$> fst <*> fst . snd)
                . filter ((== '#') . snd . snd)
                . concat
                . map (\(r, b) -> zip [0..] . map (r,) $ BS8.unpack b)
                . zip [0..]
                $ BS8.lines inp
      numA = V.length asteroids
      pairs =
        [ (k, S.singleton v)
        | a <- [0 .. numA - 1]
        , b <- [a + 1 .. numA - 1]
        , let (ar, ac) = asteroids V.! a
              (br, bc) = asteroids V.! b
              slope = mkSlope (ar - br) (ac - bc)
        , (k, v) <- [(a, slope), (b, join bimap negate slope)]
        ]
      visible = S.size <$> V.accum (<>) (V.replicate numA S.empty) pairs
    in BS8.pack . show $ V.maximum visible

dayTenB :: BS8.ByteString -> BS8.ByteString
dayTenB inp =
  let asteroids = V.fromList
                . map ((,) <$> fst <*> fst . snd)
                . filter ((== '#') . snd . snd)
                . concat
                . map (\(r, b) -> zip [0..] . map (r,) $ BS8.unpack b)
                . zip [0..]
                $ BS8.lines inp
      numA = V.length asteroids
      pairs =
        [ (k, v)
        | a <- [0 .. numA - 1]
        , b <- [a + 1 .. numA - 1]
        , let (ar, ac) = asteroids V.! a
              (br, bc) = asteroids V.! b
              slope = mkSlope (br - ar) (ac - bc)
        , (k, v) <- [ (a, M.singleton (V slope) (S.singleton $ D (br - ar, ac - bc)))
                    , (b, M.singleton (V $ join bimap negate slope)
                                      (S.singleton $ D (ar - br, bc - ac))
                      )
                    ]
        ]
      visible = V.accum (M.unionWith (<>)) (V.replicate numA M.empty) pairs
      maxIdx = V.maxIndexBy (compare `on` M.size) visible
      maxVisible = visible V.! maxIdx
      inOrder = concat . transpose . map (map getD . S.toList)
              . map snd . take (M.size maxVisible) . orient
              . cycle $ M.toList maxVisible
      (ox, oy) = asteroids V.! maxIdx
   in case splitAt 199 inOrder of
        (_, (x, y) : _) -> BS8.pack . show $ (x + ox) * 100 + (oy - y)
        _ -> "invalid input"

orient :: [(V, S.Set D)] -> [(V, S.Set D)]
orient = go False where
  go _ [] = []
  go False ((V (x, _), _) : xs)
    | x < 0 = go True xs
    | otherwise = go False xs
  go True inp@((V (x, _), _) : xs)
    | x >= 0 = inp
    | otherwise = go True xs

mkSlope :: Int -> Int -> (Int, Int)
mkSlope a b = (a `div` d, b `div` d)
  where d = gcd a b

cross :: (Int, Int) -> (Int, Int) -> Int
cross (ax, ay) (bx, by) = ax * by - ay * bx

newtype V = V { getV :: (Int, Int) } deriving (Eq, Show)

instance Ord V where
  compare = (flip compare 0 .) . cross `on` getV

newtype D = D { getD :: (Int, Int) } deriving (Eq, Show)

instance Ord D where
  compare = compare `on` (bimap abs abs . getD)
