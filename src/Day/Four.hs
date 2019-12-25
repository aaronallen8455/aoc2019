{-# LANGUAGE OverloadedStrings #-}
module Day.Four where

import           Control.Arrow
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import           Data.List (foldl', group)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Data.Word (Word8)

import           Day.Common (readInt)

dayFourA :: BS.ByteString -> BS.ByteString
dayFourA inp = fromMaybe "bad input" $ do
  [rStart, rEnd] <- Just . (map . map) (subtract 48)
                  $ BS.unpack <$> BS8.splitWith (== '-') inp
  let start = firstNum rStart
      end = lastNum rEnd
      len = length start
      result = go 0 0 end False False len
             - go 0 0 start False False len
             + 1
  pure . BS8.pack $ show result

-- Find the first valid combo from the bottom of the range
firstNum :: [Word8] -> [Word8]
firstNum = assertPair . tail . scanl max 0
  where
    assertPair inp
      | hasPair inp = inp
      | otherwise = init (init inp) ++ (replicate 2 $ last inp)

-- Find the last valid combo from the top of the range
lastNum :: [Word8] -> [Word8]
lastNum inp =
  assertPair . fst $ until (uncurry (==)) (go &&& id <<< fst) (inp, [])
  where
    go (a:b:r)
      | a > b = (a-1) : replicate (length r + 1) 9
      | otherwise = a : go (b:r)
    go [a] = [a]
    assertPair inp
      | hasPair inp = inp
      | otherwise = reverse . uncurry (:) . (head &&& id) . tail $ reverse inp

hasPair :: Eq a => [a] -> Bool
hasPair = uncurry (/=) . (length . group &&& length)

-- memoized factorials
fact :: V.Vector Int
fact = V.fromList $ scanl (*) 1 [1..100]

-- combinatoric function
choose :: Int -> Int -> Int
choose n k | k > n = 0
choose n k = fact V.! n `div` (fact V.! k * fact V.! (n - k))

-- Find the number of valid permutations where we select m of n digits
-- first param is number of digits, second is number of places
perms :: Int -> Int -> Int
perms n m = choose (n + m - 1) m

go :: Word8 -> Word8 -> [Word8] -> Bool -> Bool -> Int -> Int
go _ _ [] _ _ _ = 0
go b a (x:xs) p _ l
  | b == x = go x a xs True True (l - 1)
  | a == x = go x a xs p True (l - 1)
go _ a xs True _ l = perms (10 - fromIntegral a) (l-1)
                   + go a (a+1) xs True False l
go b a xs False False l = (perms (10 - fromIntegral a) (l - 2))
                        + allValid (a + 1) (l - 1)
                        + go b (a + 1) xs False False l
go b a xs False True l = perms (10 - fromIntegral a) (l - 1)
                       + go b (a + 1) xs False False l

allValid :: Word8 -> Int -> Int
allValid a len =
  let nd = 10 - fromIntegral a
   in sum [ nd `choose` x * (len-1) `choose` (x-1) | x <- [1 .. len - 1]]

-- can approach part 2 by shortening length by 2 and decreasing number of digits
-- by 1. Meaning 2 places must be allocated exclusively to a single digit. But
-- that digit needs to still occur oustide the pair <- no, increasing invariant.

valid2 :: Word8 -> Int -> Int
valid2 a len =
  let nd = 9 - fromIntegral a
   in sum [ nd `choose` x * (len-2) `choose` (x-1) * (x+1) | x <- [ 1 .. len - 2 ] ]
  --perms (9 - fromIntegral a) (len - 2) * (9 - fromIntegral a)

-- cheater!
comp :: Int -> Int -> Int
comp s e = length [ x | x <- [s..e], let xs = show x, hasPair xs, incr xs, part2 xs ]

incr :: Ord a => [a] -> Bool
incr xs = and $ zipWith (<=) xs (tail xs)

part2 :: Eq a => [a] -> Bool
part2 = any ((==2) . length) . group
--part2 = const True

-- if there is a limited number of digits then the method being use to capture
-- all cases of pairing results in pairing that is redundant.

-- is there some way to check that the digits are stretched too thin and use
-- a lower factor then l - 2?
