{-# LANGUAGE OverloadedStrings #-}
module Day.One
  ( dayOneA
  , dayOneB
  , calcFuel
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe)

import           Day.Common (readInt)

calcFuel :: Int -> Int
calcFuel = subtract 2 . (`div` 3)

dayOneA :: BS.ByteString -> BS.ByteString
dayOneA = fromMaybe "bad input"
        . fmap (BS.pack . show . sum)
        . traverse (fmap calcFuel . readInt)
        . BS.lines

dayOneB :: BS.ByteString -> BS.ByteString
dayOneB = fromMaybe "bad input"
        . fmap (BS.pack . show . sum)
        . traverse (fmap (sum . takeWhile (>0) . tail . iterate calcFuel) . readInt)
        . BS.lines
