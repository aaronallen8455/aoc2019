{-# LANGUAGE OverloadedStrings #-}
module Day.Eighteen
  ( dayEighteenA
  , dayEighteenB
  ) where

import           Control.Monad.Trans.State
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isLower, isUpper, toLower)
import           Data.List (sort, sortOn)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Semigroup (Min(..))
import qualified Data.Set as S
import           Safe (lastMay)

import           Day.Common (biDirectionalBFS)

dayEighteenA :: BS8.ByteString -> BS8.ByteString
dayEighteenA inp = fromMaybe "invalid input" $ do
  (maze, keys) <- parseMaze inp
  let keyPaths = M.fromListWith (<>) $ concat
                   [ [(k1, [(k2, p)]), (k2, [(k1, p)])]
                   | x@(k1, c1) <- keys
                   , (k2, c2) <- dropWhile (<= x) keys
                   , Just p <- [(fmap . S.map) toLower <$> findPaths maze c1 c2]
                   ]
      keyPaths' = sortOn (fst . snd) <$> keyPaths

  Min r <- evalState (collectKeys keyPaths' (S.singleton '@') '@' 0) M.empty

  pure . BS8.pack $ show r

dayEighteenB :: BS8.ByteString -> BS8.ByteString
dayEighteenB inp = fromMaybe "invalid input" $ do
  (maze, keys) <- parseMaze inp
  let keyPaths = M.fromListWith (<>) $ concat
                   [ [(k1, [(k2, p)]), (k2, [(k1, p)])]
                   | x@(k1, c1) <- keys
                   , (k2, c2) <- dropWhile (<= x) keys
                   , Just p <- [(fmap . S.map) toLower <$> findPaths maze c1 c2]
                   ]
      keyPaths' = sortOn (fst . snd) <$> keyPaths

  Min r <- evalState (collectKeysPar keyPaths' (S.fromList "@!$%") "@!$%" 0) M.empty

  pure . BS8.pack $ show r

collectKeys :: M.Map Char [(Char, (Int, S.Set Char))]
            -> S.Set Char
            -> Char
            -> Int
            -> State (M.Map (Char, S.Set Char) (Maybe (Min Int))) (Maybe (Min Int))
collectKeys m _ _ acc | M.size m == 1 = pure $ Just (Min acc)
collectKeys m keysInv curKey acc = do
  mbDp <- gets (M.lookup (curKey, keysInv))
  case mbDp of
    Just dp -> pure $ (+ (Min acc)) <$> dp
    _ -> do
      v <- mconcat <$> sequence
        [ collectKeys (M.delete curKey m) (S.insert k keysInv) k $! dist + acc
        | (k, (dist, ds)) <- concat $ M.lookup curKey m
        , not $ S.member k keysInv
        , ds `S.isSubsetOf` keysInv
        ]
      modify (M.insert (curKey, keysInv) $ subtract (Min acc) <$> v)
      pure v

collectKeysPar :: M.Map Char [(Char, (Int, S.Set Char))]
               -> S.Set Char
               -> String
               -> Int
               -> State (M.Map (String, S.Set Char) (Maybe (Min Int))) (Maybe (Min Int))
collectKeysPar m _ curKeys acc | M.size m == length curKeys = pure $ Just (Min acc)
collectKeysPar m keysInv curKeys acc = do
  mbDp <- gets (M.lookup (curKeys, keysInv))
  case mbDp of
    Just dp -> pure $ (+ (Min acc)) <$> dp
    _ -> do
      v <- mconcat <$> sequence
        [ collectKeysPar (M.delete curKey m) (S.insert k keysInv) (k:otherKeys) $! dist + acc
        | curKey <- curKeys
        , (k, (dist, ds)) <- concat $ M.lookup curKey m
        , not $ S.member k keysInv
        , ds `S.isSubsetOf` keysInv
        , let otherKeys = filter (/= curKey) curKeys
        ]
      modify (M.insert (curKeys, keysInv) $ subtract (Min acc) <$> v)
      pure v

type Maze = Array (Int, Int) Tile

parseMaze :: BS8.ByteString -> Maybe (Maze, [(Char, (Int, Int))])
parseMaze bs = do
    cells <- (traverse . traverse) parseTile
               [ ((x, y), cell)
               | (y, row) <- zip [0..] . map BS8.unpack $ BS8.lines bs
               , (x, cell) <- zip [0..] row
               ]
    ((w, h), _) <- lastMay cells
    let keys = mapMaybe getKey cells
    pure (array ((0, 0), (w, h)) cells, sort keys)
  where
    getKey (coord, Key c) = Just (c, coord)
    getKey _ = Nothing

data Tile
  = Empty Bool
  | Wall
  | Door Char
  | Key Char
  deriving (Eq, Show)

parseTile :: Char -> Maybe Tile
parseTile '.' = Just $ Empty False
parseTile '#' = Just Wall
parseTile c | c `elem` ("@$!%" :: String) = Just $ Key c
parseTile c
  | isUpper c = Just $ Door c
  | isLower c = Just $ Key c
  | otherwise = Nothing

findPaths :: Maze -> (Int, Int) -> (Int, Int) -> Maybe (Int, S.Set Char)
findPaths maze start end = biDirectionalBFS go (start, S.empty) (end, S.empty)
  where
    go (x, y) keys =
      [ (coord, key <> keys )
      | coord <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
      , let cell = maze ! coord
      , cell /= Wall
      , let key = case cell of
                    Door c -> S.singleton c
                    _ -> S.empty
      ]

