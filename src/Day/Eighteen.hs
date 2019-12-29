{-# LANGUAGE OverloadedStrings #-}
module Day.Eighteen where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&))
import           Control.Monad (join)
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.State
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isLower, isUpper, toLower)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Semigroup (Min(..))
import qualified Data.Set as S
import           Safe (lastMay)

import           Debug.Trace

dayEighteenA :: BS8.ByteString -> BS8.ByteString
dayEighteenA inp = fromMaybe "invalid input" $ do
  (maze, keys, start) <- parseMaze inp
  result <- evalState (collectKeys maze (S.fromList keys) ('@', start) S.empty 0) (M.empty, maxBound)
  pure . BS8.pack $ show result

-- could make this tail recursive and put the current min in state.
collectKeys :: Maze -> S.Set KeyLoc -> (Char, (Int, Int)) -> S.Set Char -> Int -> State (Distances, Int) (Maybe Int)
collectKeys _ k _ _ acc | S.null k acc = pure $ Just acc
collectKeys maze keysToGo (curKey, pos) keysInv acc = do
  stop <- modify ((< acc) . snd)
  if stop
     then pure Nothing
     else fmap getMin . mconcat <$> traverse collectKey (S.toList keysToGo)
  where
    collectKey :: KeyLoc -> State Distances (Maybe (Min Int))
    collectKey targetKey@(k, kc) = do
      dp <- get
      let fstKey = min curKey k
          sndKey = max curKey k
      case join $  M.lookup sndKey <$> M.lookup (fstKey, keysInv) dp of
        Just x -> pure $ Min <$> x
        Nothing -> do
          r <- case shortestDistance maze keysInv pos kc of
                  Nothing -> pure Nothing
                  b@(Just xx) -> do
                    collectKeys maze (S.delete targetKey keysToGo) targetKey (S.insert k keysInv) (acc + xx)
                    pure b

          modify $ M.insert (fstKey, keysInv) (M.singleton sndKey r)
          pure $ Min <$> r

-- Stores shortest distance results from one key to another.
-- has the list of keys needed for the route.
type Distances =
  M.Map (Char, S.Set Char) (M.Map Char (Maybe Int))

type Maze = Array (Int, Int) Tile

type KeyLoc = (Char, (Int, Int))

parseMaze :: BS8.ByteString -> Maybe (Maze, [(Char, (Int, Int))], (Int, Int))
parseMaze bs = do
    cells <- (traverse . traverse) parseTile
               [ ((x, y), cell)
               | (y, row) <- zip [0..] . map BS8.unpack $ BS8.lines bs
               , (x, cell) <- zip [0..] row
               ]
    ((w, h), _) <- lastMay cells
    let keys = mapMaybe getKey cells
    [start] <- Just $ mapMaybe getStart cells
    pure (array ((0, 0), (w, h)) cells, keys, start)
  where
    getKey (coord, Key c) = Just (c, coord)
    getKey _ = Nothing
    getStart (coord, Empty True) = Just coord
    getStart _ = Nothing

data Tile
  = Empty Bool
  | Wall
  | Door Char
  | Key Char
  deriving (Eq, Show)

parseTile :: Char -> Maybe Tile
parseTile '.' = Just $ Empty False
parseTile '#' = Just Wall
parseTile '@' = Just $ Empty True
parseTile c
  | isUpper c = Just $ Door c
  | isLower c = Just $ Key c
  | otherwise = Nothing

-- bidirectional BFS
shortestDistance :: Maze -> S.Set Char -> (Int, Int) -> (Int, Int) -> Maybe Int
shortestDistance maze keys start end = go [start] [end] M.empty M.empty 0 where
  go [] _ _ _ _ = Nothing
  go _ [] _ _ _ = Nothing
  go sq eq sv ev acc = (`runCont` id) . callCC $ \k -> do
    let sv' = foldr (flip M.insert acc) sv sq
        ev' = foldr (flip M.insert acc) ev eq
        search visited dest c@(x, y)
          | Just d <- M.lookup c dest = k . Just $ acc + d
          | otherwise = pure
              [ coord
              | coord <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
              , not $ M.member coord visited
              , case maze ! coord of
                  Empty _ -> True
                  Wall -> False
                  Door c -> canOpenDoor keys c
                  Key _ -> True -- track keys that are picked up on the way?
              ]
    sq' <- S.toList . S.fromList . concat <$> traverse (search sv' ev') sq
    eq' <- S.toList . S.fromList . concat <$> traverse (search ev' sv') eq
    pure . go sq' eq' sv' ev' $! acc + 1

canOpenDoor :: S.Set Char -> Char -> Bool
canOpenDoor keys door = toLower door `S.member` keys

