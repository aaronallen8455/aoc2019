{-# LANGUAGE OverloadedStrings #-}
module Day.Two
  ( dayTwoA
  , dayTwoB
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char (isNumber)
import           Data.Function (on)
import qualified Data.IntMap as M
import           Data.List (groupBy)
import           Data.Maybe (catMaybes, fromMaybe)
import           Text.Read (readMaybe)

import           Day.Common (commaSep, readInt)

dayTwoA :: BS.ByteString -> BS.ByteString
dayTwoA = fromMaybe "invalid program!" . fmap (BS.pack . show) . (M.lookup 0 =<<)
        . runProgram 12 2 . M.fromList . zip [0..] . parseInput

parseInput :: BS.ByteString -> [Int]
parseInput = catMaybes . map readInt . commaSep

runProgram :: Int -> Int -> M.IntMap Int -> Maybe (M.IntMap Int)
runProgram noun verb inp = go 0 initMap where
  initMap = M.insert 2 verb $ M.insert 1 noun inp
  go i m =
    case m M.! i of
      1 -> go (i + 4) =<< doOp (+)
      2 -> go (i + 4) =<< doOp (*)
      99 -> Just m
      _ -> Nothing
      where
        doOp op = M.insert
                    <$> M.lookup (i + 3) m
                    <*> (op <$> (flip M.lookup m =<< (M.lookup (i + 1) m))
                            <*> (flip M.lookup m =<< (M.lookup (i + 2) m))
                        )
                    <*> Just m

dayTwoB :: BS.ByteString -> BS.ByteString
dayTwoB inp = BS.pack . show . head $
  [ 100 * noun + verb
  | noun <- [0..99]
  , verb <- [noun..99]
  , Just 19690720 <- [M.lookup 0 =<< runProgram noun verb m]
  ] where
    m = M.fromList . zip [0..] $ parseInput inp
