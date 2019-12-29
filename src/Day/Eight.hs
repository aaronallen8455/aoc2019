{-# LANGUAGE OverloadedStrings #-}
module Day.Eight
  ( dayEightA
  , dayEightB
  ) where

import           Control.Arrow ((&&&))
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import           Day.Common (readInt, chunks)

dayEightA :: BS8.ByteString -> BS8.ByteString
dayEightA inp = fromMaybe "invalid input" $ do
  inp' : _ <- Just $ BS8.lines inp
  cs <- (traverse . traverse) readInt
      . map (chunks 1) $ chunks (6 * 25) inp'
  let layers = layer <$> cs
      min0 = snd . minimum $ map ((V.! 0) &&& id) layers
  pure . BS8.pack . show $ min0 V.! 1 * min0 V.! 2

dayEightB :: BS8.ByteString -> BS8.ByteString
dayEightB inp = fromMaybe "invalid input" $ do
  inp' : _ <- Just $ BS8.lines inp
  let cs = foldr1 (\x -> BS8.pack . BS8.zipWith fillPixel x) $ chunks (6 * 25) inp'
      lns = chunks 25 cs
  pure $ BS8.unlines lns

layer :: [Int] -> V.Vector Int
layer xs = V.accum (+) (V.replicate (length xs) 0) (zip xs $ repeat 1)

fillPixel :: Char -> Char -> Char
fillPixel '2' x = x
fillPixel x _ = x
