{-# LANGUAGE OverloadedStrings #-}
module Day.Nineteen where

import           Control.Monad (join)
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IM
import           Data.Maybe (fromMaybe)

import           Day.IntCode (parseInput, runIntCodeProgram)

dayNineteenA :: BS8.ByteString -> BS8.ByteString
dayNineteenA inp = fromMaybe "bad input" $ do
  codes <- parseInput inp

  rows <- sequence [ runIntCodeProgram [x, y] codes | x <- [0..49], y <- [0..49]]
  let c = length . filter (==1) $ concat rows
  pure . BS8.pack $ show c

dayNineteenB :: BS8.ByteString -> BS8.ByteString
dayNineteenB inp = fromMaybe "bad input" $ do
  codes <- parseInput inp
  slopeA <- aproxSlope codes False (0, 10)
  slopeB <- (1 /) <$> aproxSlope codes True (0, 10)
  let r = findShipLoc slopeA slopeB
  pure . BS8.pack $ show r

aproxSlope :: IM.IntMap Int -> Bool -> (Int, Int) -> Maybe Double
aproxSlope _ _ (x, y) | x > 500000000 = Just $ fromIntegral y / fromIntegral x
aproxSlope codes flipped coords =
  let rev = if flipped then reverse else id
      go (x, y) (Just True) =
        case runIntCodeProgram (rev [x-1, y]) codes of
          Just [1] -> go (x - 1, y) $ Just True
          Just [0] -> Just (x, y)
          _ -> Nothing
      go (x, y) (Just False) =
        case runIntCodeProgram (rev [x+1, y]) codes of
          Just [1] -> Just (x + 1, y)
          Just [0] -> go (x + 1, y) $ Just False
          _ -> Nothing
      go (x, y) Nothing =
        case runIntCodeProgram (rev [x, y]) codes of
          Just [1] -> go (x, y) $ Just True
          Just [0] -> go (x, y) $ Just False
          _ -> Nothing
   in go coords Nothing >>= aproxSlope codes flipped . join bimap (*100)

findShipLoc :: Double -> Double -> Int
findShipLoc slopeA slopeB =
  let x = (99 + 99 * slopeB) / (slopeA - slopeB)
      y = slopeA * x - 99
      adjust :: Int -> Int -> (Int, Int)
      adjust x' y'
        | fromIntegral y' + 99 > slopeA * fromIntegral x' = adjust (x' + 1) y'
        | fromIntegral y' < slopeB * (fromIntegral x' + 99) = adjust x' (y' + 1)
        | otherwise = (x', y')
      (x'', y'') = adjust (ceiling x) (ceiling y)
   in x'' * 10000 + y''

