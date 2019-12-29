{-# LANGUAGE OverloadedStrings #-}
module Day.Thirteen
  ( dayThirteenA
  , dayThirteenB
  ) where

import           Control.Arrow ((***), first)
import           Control.Monad.Fix (mfix)
import           Control.Monad.Trans.State
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Monoid (Last(..))
import qualified Data.Set as S

import           Day.IntCode (parseInput, runIntCodeProgram', runIntCodeProgram)

import           Debug.Trace

dayThirteenA :: BS8.ByteString -> BS8.ByteString
dayThirteenA inp = fromMaybe "invalid  program" $ do
  codes <- parseInput inp
  output <- runIntCodeProgram [] codes
  (screen, _) <- buildScreen output
  traceShow (filter ((== Ball) . snd) $ assocs screen) pure ()
  traceShow (filter ((== Paddle) . snd) $ assocs screen) pure ()
  pure . BS8.pack . show . length . filter (== Block) $ elems screen

dayThirteenB :: BS8.ByteString -> BS8.ByteString
dayThirteenB inp = fromMaybe "invalid program" $ do
  codes <- IM.insert 0 2 <$> parseInput inp
  let output = runIntCodeProgram' codes input
      input = handleOutput output
  score : _ <- Just $ reverse output
  pure . BS8.pack $ show score

-- screen is 23 tall, 39 wide
buildScreen :: [Int] -> Maybe (Screen, Maybe Int)
buildScreen inp = do
  (instr, mbScore) <- parseInstructions inp
  pure (array ((0, 0), (39, 23)) instr, mbScore)

parseInstructions :: [Int] -> Maybe ([((Int, Int), Tile)], Maybe Int)
parseInstructions (-1 : 0 : s : xs) =
  (id *** const (Just s)) <$> parseInstructions xs
parseInstructions (a : b : c : xs) = do
  t <- parseTile c
  ((((a, b), t) :) *** id) <$> parseInstructions xs
parseInstructions [] = Just ([], Nothing)
parseInstructions _ = Nothing

type Screen = Array (Int, Int) Tile

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Show, Eq)

handleOutput :: [Int] -> [Int]
handleOutput = go Nothing S.empty where
  go :: Maybe Int -> S.Set (Int, Int) -> [Int] -> [Int]
  go paddle blocks (x : _ : 4 : xs)
    | Just px <- paddle =
        let o | x == px = 0
              | x < px = -1
              | x > px = 1
         in o : go Nothing blocks xs
    | otherwise = 0 : go paddle blocks xs
  go _ blocks (x : _ : 3 : xs)
    | otherwise = go (Just x) blocks xs
  go paddle blocks (-1 : 0 : score : xs) =
    if S.size blocks == 0
       then []
       else go paddle blocks xs
  go paddle blocks (x : y : 2 : xs) = do
    go paddle (S.insert (x, y) blocks) xs
  go paddle blocks (x : y : 0 : xs) =
    go paddle (S.delete (x, y) blocks) xs
  go paddle blocks (_ : _ : _ : xs) = go paddle blocks xs
  go _ _ [] = []

parseBallOrPaddle :: Int -> Maybe Tile
parseBallOrPaddle 4 = Just Ball
parseBallOrPaddle 3 = Just Paddle
parseBallOrPaddle _ = Nothing

parseTile :: Int -> Maybe Tile
parseTile i = case i of
                0 -> Just Empty
                1 -> Just Wall
                2 -> Just Block
                3 -> Just Paddle
                4 -> Just Ball
                _ -> Nothing
