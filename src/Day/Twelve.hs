{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Day.Twelve
  ( dayTwelveA
  , dayTwelveB
  ) where

import           Control.Comonad
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import           Day.Common (readInt)

dayTwelveA :: BS8.ByteString -> BS8.ByteString
dayTwelveA inp = fromMaybe "invalid input" $ do
  planets <- traverse vecParser $ BS8.lines inp
  let sys = System (V.fromList $ repeat (Vec 0 0 0) `zip` planets) 0
  finalSys : _ <- Just . drop 1000 $ iterate (extend step) sys
  let (System v _) = extend totalEnergy finalSys
  pure . BS8.pack . show $ V.sum v

dayTwelveB :: BS8.ByteString -> BS8.ByteString
dayTwelveB inp = fromMaybe "invalid input" $ do
  planets <- traverse vecParser $ BS8.lines inp
  let x = V.fromList $ repeat 0 `zip` map vx planets
      y = V.fromList $ repeat 0 `zip` map vy planets
      z = V.fromList $ repeat 0 `zip` map vz planets
  (xo, xl) <- findLoop $ iterate (extend stepAxis) (System x 0)
  (yo, yl) <- findLoop $ iterate (extend stepAxis) (System y 0)
  (zo, zl) <- findLoop $ iterate (extend stepAxis) (System z 0)
  pure . BS8.pack . show $ maximum [xo, yo, zo] + (xl `lcm` yl `lcm` zl)

vecParser :: BS8.ByteString -> Maybe Vec
vecParser bs
  | (b1, z) <- BS8.breakEnd (== '=') $ BS8.init bs
  , (b2, (y, _)) <- BS8.break (== ',') <$> BS8.breakEnd (== '=') (BS8.init b1)
  , (_, (x, _)) <- BS8.break (== ',') <$> BS8.breakEnd (== '=') (BS8.init b2)
  = Vec <$> readInt x <*> readInt y <*> readInt z
  | otherwise = Nothing

data Vec =
  Vec
    { vx :: Int
    , vy :: Int
    , vz :: Int
    } deriving (Eq, Show)

addVec :: Vec -> Vec -> Vec
addVec (Vec a b c) (Vec a' b' c') = Vec (a + a') (b + b') (c + c')

data System a =
  System (V.Vector a) Int
  deriving (Show, Functor, Eq, Ord)

instance Comonad System where
  extract (System v i) = v V.! i
  duplicate (System v i) = System v' i where
    v' = V.fromList [ System v i' | i' <- [0 .. V.length v - 1] ]

step :: System (Vec, Vec) -> (Vec, Vec)
step (System v i) = (newV, newP) where
  (oldV, oldP@(Vec px py pz)) = v V.! i
  newV = foldr go oldV [ snd $ v V.! d | d <- [ 0 .. V.length v - 1 ], d /= i ]
  go (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x2 + dx) (y2 + dy) (z2 + dz)
    where
      dx = delta px x1
      dy = delta py y1
      dz = delta pz z1
  delta p1 p2
    | p1 == p2 = 0
    | p1 > p2 = -1
    | otherwise = 1
  newP = oldP `addVec` newV

totalEnergy :: System (Vec, Vec) -> Int
totalEnergy (System v i) = sumVec vel * sumVec pos where
  (vel, pos) = v V.! i
  sumVec (Vec x y z) = sum $ abs <$> [x, y, z]

stepAxis :: System (Int, Int) -> (Int, Int)
stepAxis (System v i) = (newV, newP) where
  (oldV, oldP) = v V.! i
  newV = foldr go oldV [ snd $ v V.! d | d <- [ 0 .. V.length v - 1 ], d /= i ]
  newP = oldP + newV
  go p v'
    | p == oldP = v'
    | p > oldP = v' + 1
    | otherwise = v' - 1

-- find the offset and the length of the loop from a potentially infinite list
-- of velocity, position.
findLoop :: [System (Int, Int)] -> Maybe (Int, Int)
findLoop steps = foldr go (\_ _ -> Nothing) steps 0 M.empty
  where
    go s acc !idx m
      | Just i <- M.lookup s m = Just (i, idx - i)
      | otherwise = acc (idx + 1) (M.insert s idx m)

