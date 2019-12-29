{-# LANGUAGE OverloadedStrings #-}
module Day.Eighteen where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&), first, second)
import           Control.Monad (join)
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.State
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (isLower, isUpper, toLower)
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
  let keyToKey = mempty -- M.fromList
--        [ ((k1, k2), (d, ks))
--        | x@(k1, c1) <- keys
--        , (k2, c2) <- dropWhile (<= x) keys
--        , Just (d, ks) <- [shortestWithKey maze c1 c2]
--        ]

  result <- evalState (collectKeys maze keyToKey (S.fromList keys) ('@', start) S.empty 0) (Ctx maxBound M.empty M.empty)
  pure . BS8.pack $ show result

-- could make this tail recursive and put the current min in state.
collectKeys :: Maze -> KeyToKey -> S.Set KeyLoc -> (Char, (Int, Int)) -> S.Set Char -> Int -> State Ctx (Maybe Int)
collectKeys _ _ k _ _ acc | S.null k = do
  curMin <- gets ctxMin
  if acc >= curMin
     then pure Nothing
     else traceShow acc $ Just acc <$ modify (\c -> c { ctxMin = min (ctxMin c) acc })
collectKeys maze keyToKey keysToGo (curKey, pos) keysInv acc = do
  curMin <- gets ctxMin
  if acc >= curMin
     then pure Nothing
     else do
       dp <- gets ctxDP
       let keyList = S.toList keysInv
       case M.lookup (curKey, keyList) dp of
         Just mbR -> pure $ (+) <$> mbR <*> Just acc
         Nothing -> do
           r <- fmap getMin . mconcat <$> traverse collectKey (S.toList keysToGo)
           dp' <- gets ctxDP
           let dp'' = M.insert (curKey, keyList) (subtract acc <$> r) dp'
           modify (\c -> c { ctxDP = dp'' })
           pure r
  where
    collectKey :: KeyLoc -> State Ctx (Maybe (Min Int))
    collectKey targetKey@(k, kc) = do
      let fstKey = min curKey k
          sndKey = max curKey k
      mbDist <- --case M.lookup (fstKey, sndKey) keyToKey of
                --     Just (dist, req)
                --       | req `S.isSubsetOf` keysInv -> pure $ Just dist
                      shortestDistance maze keysInv pos kc
      dist <- gets ctxDistance
      traceShow dist pure ()

      r <- for mbDist $ \d -> do
             let acc' = acc + d
             collectKeys maze keyToKey (S.delete targetKey keysToGo) targetKey (S.insert k keysInv) $! acc'

      pure $ Min <$> join r

type KeyToKey = M.Map (Char, Char) (Int, S.Set Char)

type Distance = M.Map ((Int, Int), (Int, Int), S.Set Char) (Maybe Int)

type DP = M.Map (Char, String) (Maybe Int)

type Maze = Array (Int, Int) Tile

type KeyLoc = (Char, (Int, Int))

data Ctx =
  Ctx
    { ctxMin :: Int
    , ctxDP :: DP
    , ctxDistance :: Distance
    }

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
shortestDistance :: Maze -> S.Set Char -> (Int, Int) -> (Int, Int) -> State Ctx (Maybe Int)
shortestDistance maze keys start end = do
  dp <- gets ctxDistance
  let fstC = min start end
      sndC = max start end
  case M.lookup (fstC, sndC, keys) dp of
    Just x -> error "fuck you" -- pure x
    Nothing -> do
      let r = go [start] [end] M.empty M.empty 0
          dp' = M.insert (fstC, sndC, keys) r dp
      modify (\c -> c { ctxDistance = dp' })
      pure r
  where
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
