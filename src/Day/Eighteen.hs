{-# LANGUAGE OverloadedStrings #-}
module Day.Eighteen where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&), first, second)
import           Control.Monad (join)
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Array
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isLower, isUpper, toLower)
import qualified Data.DList as DL
import           Data.Either (partitionEithers)
import           Data.List (sort)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Semigroup (Min(..))
import qualified Data.Set as S
import           Data.Traversable (for)
import           Safe (lastMay)

import           Debug.Trace

dayEighteenA :: BS8.ByteString -> BS8.ByteString
dayEighteenA inp = fromMaybe "invalid input" $ do
  (maze, keys, start) <- parseMaze inp
  let keyPaths = M.fromList
                   [ ((k1, k2), p)
                   | x@(k1, c1) <- keys
                   , (k2, c2) <- dropWhile (<= x) keys
                   , let p = findPaths maze c1 c2
                   ]
  traceShow keyPaths pure ()
  pure "test"
--  let keyToKey = M.fromList
--        [ ((k1, k2), (d, ks))
--        | x@(k1, c1) <- keys
--        , (k2, c2) <- dropWhile (<= x) keys
--        , Just (d, ks) <- [shortestWithKey maze c1 c2]
--        ]
--
--  result <- evalState (collectKeys maze keyToKey (S.fromList keys) ('@', start) S.empty 0) (maxBound, M.empty)
--  pure . BS8.pack $ show result

-- could make this tail recursive and put the current min in state.
collectKeys :: Maze -> KeyToKey -> S.Set KeyLoc -> (Char, (Int, Int)) -> S.Set Char -> Int -> State (Int, Distance) (Maybe Int)
collectKeys _ _ k _ _ acc | S.null k = traceShow acc $ Just acc <$ modify (first $ const acc)
collectKeys maze keyToKey keysToGo (curKey, pos) keysInv acc =
  fmap getMin . mconcat <$> traverse collectKey (S.toList keysToGo)
  where
    collectKey :: KeyLoc -> State (Int, Distance) (Maybe (Min Int))
    collectKey targetKey@(k, kc) = do
      let fstKey = min curKey k
          sndKey = max curKey k
          invKeyList = S.toList keysInv
      dp <- gets snd
      mbD <- case M.lookup (fstKey, sndKey, invKeyList) dp of
        Nothing -> do
          let mbDist = case M.lookup (fstKey, sndKey) keyToKey of
                         Just (dist, req)
                           | req `S.isSubsetOf` keysInv -> Just dist
                         _ -> shortestDistance maze keysInv pos kc
          modify . second $ M.insert (fstKey, sndKey, invKeyList) mbDist
          pure mbDist
        Just x -> pure x

      r <- for mbD $ \d -> do
             curMin <- gets fst
             let acc' = acc + d
             if acc' >= curMin
                then pure Nothing
                else do
                  collectKeys maze keyToKey (S.delete targetKey keysToGo) targetKey (S.insert k keysInv) $! acc'

      pure $ Min <$> join r

type KeyToKey = M.Map (Char, Char) (Int, S.Set Char)

type KeyToKeyDP = M.Map (Char, Char) [(Int, S.Set Char)]

type Distance = M.Map (Char, Char, String) (Maybe Int)

type DP = M.Map (Char, String) (Maybe Int)

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
    pure (array ((0, 0), (w, h)) cells, sort keys, start)
  where
    getKey (coord, Key c) = Just (c, coord)
    getKey _ = Nothing
    getStart (coord, Key '@') = Just coord
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
parseTile '@' = Just $ Key '@'
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


shortestWithKey :: Maze -> (Int, Int) -> (Int, Int) -> Maybe (Int, S.Set Char)
shortestWithKey maze start end = go [(start, S.empty)] [(end, S.empty)] M.empty M.empty 0 where
  go [] _ _ _ _ = Nothing
  go _ [] _ _ _ = Nothing
  go sq eq sv ev acc = (`runCont` id) . callCC $ \k -> do
    let sv' = foldr (\(c, ks) -> M.insert c (acc, ks)) sv sq
        ev' = foldr (\(c, ks) -> M.insert c (acc, ks)) ev eq
        search visited dest (c@(x, y), keys)
          | Just (d, ks) <- M.lookup c dest = k . Just $ (acc + d, keys <> ks)
          | otherwise = pure
              [ (coord, keys <> key)
              | coord <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
              , not $ M.member coord visited
              , case maze ! coord of
                  Empty _ -> True
                  Wall -> False
                  Door c -> True
                  Key _ -> True -- track keys that are picked up on the way?
              , let key = case maze ! coord of
                            Door c -> S.singleton c
                            _ -> S.empty
              ]
    sq' <- S.toList . S.fromList . concat <$> traverse (search (fst <$> sv') ev') sq
    eq' <- S.toList . S.fromList . concat <$> traverse (search (fst <$> ev') sv') eq
    pure . go sq' eq' sv' ev' $! acc + 1

findPaths :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, S.Set Char)]
findPaths maze start end = DL.toList $ go [(start, S.empty)] [(end, S.empty)] M.empty M.empty 0 where
  go :: [((Int, Int), S.Set Char)]
     -> [((Int, Int), S.Set Char)]
     -> M.Map (Int, Int) [(Int, S.Set Char)]
     -> M.Map (Int, Int) [(Int, S.Set Char)]
     -> Int
     -> DL.DList (Int, S.Set Char)
  go [] _ _ _ _ = mempty
  go _ [] _ _ _ = mempty
  go sq eq sv ev acc =
    let search :: M.Map (Int, Int) [(Int, S.Set Char)]
               -> M.Map (Int, Int) [(Int, S.Set Char)]
               -> ((Int, Int), S.Set Char)
               -> ( Either [((Int, Int), S.Set Char)]
                           [(Int, S.Set Char)]
                  )
        search visited dest (c@(x, y), keys) =
          case M.lookup c dest of
            Just ds
              -> Right [ (acc + d, keys <> k) | (d, k) <- ds ]
            _ -> Left
                   [ (coord, keys <> key)
                   | coord <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                   , case maze ! coord of
                       Wall -> False
                       _ -> True
                   , let key = case maze ! coord of
                                 Door c -> S.singleton c
                                 _ -> S.empty
                   ]
        (sq', rs) = bimap concat concat . partitionEithers $ map (search sv ev) sq
        eq' = concat . fst . partitionEithers $ map (search ev sv) eq
        (sv', sq'') = foldr addToMap (sv, []) sq'
        (ev', eq'') = foldr addToMap (ev, []) eq'
        -- filters and adds to map
        addToMap cur@(c, ks) (m, q)
          | Just xs <- M.lookup c m =
            if any (flip S.isSubsetOf ks . snd) xs
               then (m, q)
               else (M.insert c ((acc + 1, ks) : xs) m, cur : q)
          | otherwise = (M.insert c [(acc + 1, ks)] m, cur : q)
      in DL.fromList rs <> (go sq'' eq'' sv' ev' $! acc + 1)
