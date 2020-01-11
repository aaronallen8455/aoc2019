{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Day.TwentyTwo where

import qualified Data.ByteString.Char8 as BS8
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)

import           Day.Common (readInt)

import           Debug.Trace

dayTwentyTwoA :: BS8.ByteString -> BS8.ByteString
dayTwentyTwoA inp = fromMaybe "invalid input" $ do
  shuffles <- traverse parseSh $ BS8.lines inp
  let (r, _) = foldl' shuffle (2019, 10007) shuffles
  pure . BS8.pack $ show r

dayTwentyTwoB :: BS8.ByteString -> BS8.ByteString
dayTwentyTwoB inp = fromMaybe "invalid input" $ do
  shuffles <- fmap reverse . traverse parseSh $ BS8.lines inp
  let (r, _) = foldl' revShuffle (2020, 119315717514047) shuffles
  pure . BS8.pack $ show r

parseShuffle :: BS8.ByteString -> Maybe (Int -> Int -> Int)
parseShuffle bs
  | Just n <- readInt =<< BS8.stripPrefix "cut " bs
  = Just $ \l -> cut l n
  | Just n <- readInt =<< BS8.stripPrefix "deal with increment " bs
  = Just $ \l -> increment l n
  | bs == "deal into new stack"
  = Just newStack
  | otherwise = Nothing

parseShuffleRev :: BS8.ByteString -> Maybe (Int -> Int -> Int)
parseShuffleRev bs
  | Just n <- readInt =<< BS8.stripPrefix "cut " bs
  = Just $ \l -> cutRev l n
  | Just n <- readInt =<< BS8.stripPrefix "deal with increment " bs
  = Just $ \l -> incrementRev l n
  | bs == "deal into new stack"
  = Just newStack
  | otherwise = Nothing

newStack :: Int -> Int -> Int
newStack l i = l - 1 - i

cut :: Int -> Int -> Int -> Int
cut l n i | n < 0 = cut l (l + n) i
cut l n i
  | i >= n = i - n
  | otherwise = l - (n - i)

cutRev :: Int -> Int -> Int -> Int
cutRev l n = cut l (negate n)

increment :: Int -> Int -> Int -> Int
increment l n i = n * i `mod` l

incrementRev :: Int -> Int -> Int -> Int
incrementRev l n i =
  let (m, d) = l `divMod` n
      (e, v) = i `divMod` n
      go 0 = 0
      go v | v >= n = 1 + go (v - n) -- too early?
      go v = m + go (v + d)
   in go v + e

data Shuffle
  = Deal
  | Cut Integer
  | Increment Integer

parseSh :: BS8.ByteString -> Maybe Shuffle
parseSh "deal into new stack" = Just Deal
parseSh bs | Just n <- readInt =<< BS8.stripPrefix "cut " bs
           = Just . Cut $ fromIntegral n
           | Just n <- readInt =<< BS8.stripPrefix "deal with increment " bs
           = Just . Increment $ fromIntegral n

shuffle :: (Integer, Integer) -> Shuffle -> (Integer, Integer)
shuffle (acc, l) Deal = ((l - 1 - acc) `mod` l, l)
shuffle (acc, l) (Cut n) = ((acc - n) `mod` l, l)
shuffle (acc, l) (Increment n) = (acc * n `mod` l, l)

revShuffle :: (Integer, Integer) -> Shuffle -> (Integer, Integer)
revShuffle (acc, l) Deal = ((l - 1 - acc) `mod` l, l)
revShuffle (acc, l) (Cut n) = ((acc + n) `mod` l, l)
revShuffle (acc, l) (Increment n) = (acc * fastExp l n (l-2) `mod` l, l)

fastExp :: Integer -> Integer -> Integer -> Integer
fastExp m x 0 = 1
fastExp m !x !e
  | even e = fastExp m (x^2 `mod` m) (e `div` 2)
  | odd e = x * fastExp m x (e - 1) `mod` m


-- to do increment backwards, can use fermats little theorem
-- which states that a^(m-2)*a = 1.
-- so we would have acc * m^(p-2) as long as p is a prime number

-- l `mod` n gives the delta from one cycle to the next
-- n `div` d is number of cycles with a + 1
--
-- when the point is reached where subtracting d will be a negative number,
-- the next cycle will start on n + negative d
--
-- so for 10007 n=64 (d=23), second cycle starts 41, third 18,
-- next would be -5 so we go to 64 - 5 = 59

--incrementRev :: Int -> Int -> Int -> Int
--incrementRev _ _ 0 = 0
--incrementRev l n i | i < 0 = incrementRev l n (i + n)
--incrementRev l n i = 1 + incrementRev l n (i - n)

-- is n always a factor of l - 1?

-- subtract n in modular space until the value n is reached
