{-# LANGUAGE OverloadedStrings #-}
module Day.Sixteen
  ( daySixteenA
  , daySixteenB
  ) where

import           Control.Comonad
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Data.Word (Word8)

import           Day.Common (chunks, readInt)

import           Debug.Trace

daySixteenA :: BS8.ByteString -> BS8.ByteString
daySixteenA inp = fromMaybe "invalid input" $ do
  stream <- traverse readInt . init $ chunks 1 inp
  (result : _) <- Just . drop 100 $ iterate phase stream
  traceShow result pure ()
  pure . BS8.pack .  concatMap show $ take 8 result

daySixteenB :: BS8.ByteString -> BS8.ByteString
daySixteenB inp = fromMaybe "invalid input" $ do
  let offset = 5970837
  stream <- V.fromList . drop offset . concat
          . replicate 10000 . map fromIntegral
        <$> traverse readInt (init $ chunks 1 inp)
  traceShow (V.length stream) pure ()
  (result : _) <- Just . drop 100 $ iterate phaseDp stream
  pure . BS8.pack .  concatMap show . take 8 $ V.toList result

phase :: [Int] -> [Int]
phase xs = phaseDigit <$> zipWith const [0..] xs <*> pure xs

phaseDigit :: Int -> [Int] -> Int
phaseDigit i = (`mod` 10) . abs . sum . zipWith (*) pattern
  where
    pattern = tail . cycle $ replicate (i + 1) =<< [0, 1, 0, -1]

-- boring problem meets brute force
phaseDp :: V.Vector Word8 -> V.Vector Word8
phaseDp prev = V.init phased where
  phased = V.scanr' go 0 prev
  go x acc = abs $ (x + acc + 10) `mod` 10
  len = V.length prev
