{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Day.Fourteen
  ( dayFourteenA
  , dayFourteenB
  ) where

import           Control.Monad (foldM)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Safe (minimumMay)
import           Text.Parsec
import           Text.Parsec.ByteString (Parser)
import           Text.Read (readMaybe)

dayFourteenA :: BS8.ByteString -> BS8.ByteString
dayFourteenA inp = fromMaybe "invalid input" $ do
  compMap <- either (const Nothing) (Just . mconcat)
           $ traverse (parse parseLine "") (BS8.lines inp)

  (result, _) <- reduceToOre compMap M.empty "FUEL" 1
  pure . BS8.pack $ show result

dayFourteenB :: BS8.ByteString -> BS8.ByteString
dayFourteenB inp = fromMaybe "invalid input" $ do
  compMap <- either (const Nothing) (Just . mconcat)
           $ traverse (parse parseLine "") (BS8.lines inp)

  result <- findMax compMap (10^(12 :: Int))

  pure . BS8.pack $ show result

type Components = (Int, [(String, Int)])

parseLine :: Parser (M.Map String [Components])
parseLine = do
  inputs <- sepBy1 resPairP (string ", ")
  _ <- string " => "
  (outName, outCount) <- resPairP
  eof
  pure $ M.singleton outName [(outCount, inputs)]
  where
    resPairP = do
      Just cnt <- readMaybe <$> many1 digit
      _ <- char ' '
      name <- many1 letter
      pure (name, cnt)

-- the minimum amount of ore needed to produce a given amount of fuel and amounts
-- of starter material
reduceToOre :: M.Map String [Components] -> M.Map String Int -> String -> Int -> Maybe (Int, M.Map String Int)
reduceToOre _ exc "ORE" total = Just (total, exc)
reduceToOre m exc key total = minimumMay $ do
  let (total', remExc) =
        case M.lookup key exc of
          Nothing -> (total, 0)
          Just x | x > total -> (0, x - total)
                 | otherwise -> (total - x, 0)

  (cnt, pieces) <- concat $ M.lookup key m

  let batches = case total' `divMod` cnt of
                  (b, 0) -> b
                  (b, _) -> b + 1

  let exc' = M.insert key (remExc + cnt * batches - total') exc
  Just (subs, exc'') <-
    [ foldM (\(i, e) (a, b) -> first (+ i) <$> reduceToOre m e a (b * batches))
            (0, exc')
            pieces
    ]

  pure $ (subs, exc'')

-- does a binary search for the max amount of fuel producible from a given ore count.
findMax :: M.Map String [Components] -> Int -> Maybe Int
findMax m ore = go 10000 0 Nothing where
  go fuel lbound mbRbound = do
    (result, _) <- reduceToOre m M.empty "FUEL" fuel
    case compare result ore of
      GT | fuel == lbound + 1
           -> Just lbound
         | otherwise
           -> go ((lbound + fuel) `div` 2) lbound (Just fuel)
      LT | Just rbound <- mbRbound
         , fuel == rbound - 1
           -> Just fuel
         | Just rbound <- mbRbound
           -> go ((fuel + rbound) `div` 2) fuel mbRbound
         | otherwise
           -> go (fuel ^ (2 :: Int)) fuel Nothing
      EQ -> Just fuel
