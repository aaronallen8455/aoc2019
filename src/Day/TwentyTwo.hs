{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Day.TwentyTwo
  ( dayTwentyTwoA
  , dayTwentyTwoB
  ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)

import           Day.Common (readInt)

dayTwentyTwoA :: BS8.ByteString -> BS8.ByteString
dayTwentyTwoA inp = fromMaybe "invalid input" $ do
  shuffles <- traverse parseShuffle $ BS8.lines inp
  let r = foldl' (shuffle 10007) 2019 shuffles
  pure . BS8.pack $ show r

dayTwentyTwoB :: BS8.ByteString -> BS8.ByteString
dayTwentyTwoB inp = fromMaybe "invalid input" $ do
  shuffles <- fmap reverse . traverse parseShuffle $ BS8.lines inp
  let l = 119315717514047
      t = 101741582076661
      (coef, m) = foldl' (simplify l) (1, 0) shuffles
      coef' = fastExp l coef t
      m' = (sumOfPowers l coef (t - 1) * m) `mod` l
      r = ((coef' * 2020 `mod` l) + m') `mod` l
  pure . BS8.pack $ show r

data Shuffle
  = Deal
  | Cut Integer
  | Increment Integer

parseShuffle :: BS8.ByteString -> Maybe Shuffle
parseShuffle "deal into new stack" = Just Deal
parseShuffle bs
  | Just n <- readInt =<< BS8.stripPrefix "cut " bs
  = Just . Cut $ fromIntegral n
  | Just n <- readInt =<< BS8.stripPrefix "deal with increment " bs
  = Just . Increment $ fromIntegral n
  | otherwise = Nothing

shuffle :: Integer -> Integer -> Shuffle -> Integer
shuffle l acc Deal = (l - 1 - acc) `mod` l
shuffle l acc (Cut n) = (acc - n) `mod` l
shuffle l acc (Increment n) = acc * n `mod` l

revShuffle :: Integer -> Integer -> Shuffle -> Integer
revShuffle l acc Deal = (l - 1 - acc) `mod` l
revShuffle l acc (Cut n) = (acc + n) `mod` l
revShuffle l acc (Increment n) = acc * fastExp l n (l-2) `mod` l

fastExp :: Integer -> Integer -> Integer -> Integer
fastExp m x 0 = 1
fastExp m !x !e
  | even e = fastExp m (x^2 `mod` m) (e `div` 2)
  | odd e = x * fastExp m x (e - 1) `mod` m

simplify :: Integer -> (Integer, Integer) -> Shuffle -> (Integer, Integer)
simplify l (!coef, !m) s =
  case s of
    Deal -> (negate coef, (l - 1 - m) `mod` l) -- this is the problem
    Cut n -> (coef, (m + n) `mod` l)
    Increment n ->
      let inv = fastExp l n (l - 2)
       in (coef * inv `mod` l, m * inv `mod` l)

-- to do increment backwards, can use fermats little theorem
-- which states that a^(m-2)*a = 1.
-- so we would have acc * m^(p-2) as long as p is a prime number

-- v^0 + v^1 + v^3 .. v^n = (v^(n+1) - 1) / (v - 1)
sumOfPowers :: Integer -> Integer -> Integer -> Integer
sumOfPowers m v n =
  ((fastExp m v (n + 1) - 1) `mod` m) * fastExp m (v - 1) (m - 2)
