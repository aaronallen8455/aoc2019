{-# LANGUAGE OverloadedStrings #-}
module Day.Thirteen
  ( dayThirteenA
  , dayThirteenB
  ) where

import           Control.Arrow ((***), first)
import           Control.Monad.Fix (mfix)
import           Control.Monad.Trans.State.Lazy
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Last(..))
import qualified Data.Set as S

import           Day.IntCode (parseInput, runIntCodeProgram)

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
  score <- snd . snd . (`runState` (S.empty, Nothing)) . mfix $ \mbOutput ->
        flip runIntCodeProgram codes
          <$> handleOutput (concat mbOutput)
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

handleOutput :: [Int] -> State (S.Set (Int, Int), Maybe Int) [Int]
handleOutput = trace "test" $ go Nothing Nothing where
  go (Just bx) (Just px) xs = trace "1" $ (o :) <$> go Nothing Nothing xs where
    o | bx == px = 0
      | bx < px = -1
      | bx > px = 1
  go _ paddle (x : _ : 4 : xs) = trace "2" $ go (Just x) paddle xs
  go ball _ (x : _ : 3 : xs) = trace "3" $ go ball (Just x) xs
  go ball paddle (-1 : 0 : s : xs) = do
    (blocks, _) <- get
    put (blocks, Just s)
    traceShow (S.size blocks) pure ()
    if S.size blocks == 0
       then pure $ repeat 0
       else go ball paddle xs
  go ball paddle (x : y : 2 : xs) = trace "4" $ do
    modify $ first (S.insert (x, y))
    go ball paddle xs
  go ball paddle (x : y : 0 : xs) = trace "5" $ do
    modify $ first (S.delete (x, y))
    go ball paddle xs
  go ball paddle (_ : _ : _ : xs) = trace "6" $ go ball paddle xs
  go _ _ [] = trace "7" $ pure []

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
