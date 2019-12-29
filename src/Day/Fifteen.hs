{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Day.Fifteen
  ( dayFifteenA
  , dayFifteenB
  ) where

import           Control.Monad.Trans.Cont
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)

import           Day.IntCode (parseInput, runIntCodeProgram'', Command(..))

dayFifteenA :: BS8.ByteString -> BS8.ByteString
dayFifteenA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  result <- fst <$> findTarget S.empty [((0, 0), runIntCodeProgram'' codes)] 0
  pure . BS8.pack $ show result

dayFifteenB :: BS8.ByteString -> BS8.ByteString
dayFifteenB inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  start <- snd <$> findTarget S.empty [((0, 0), runIntCodeProgram'' codes)] 0
  result <- fillChamber S.empty [start] 0
  pure . BS8.pack $ show result

data Response
  = Wall
  | Moved
  | AtTarget
  deriving (Eq, Show)

parseResponse :: Int -> Maybe Response
parseResponse 0 = Just Wall
parseResponse 1 = Just Moved
parseResponse 2 = Just AtTarget
parseResponse _ = Nothing

type Loc = ((Int, Int), Command)

findTarget :: S.Set (Int, Int) -> [Loc] -> Int -> Maybe (Int, Loc)
findTarget _ [] _ = Nothing
findTarget visited locs !acc = (`runCont` id) . callCC $ \k -> do
  let go ((x, y), Input c) =
        if S.member (x, y) visited
           then pure []
           else sequence
                  [ case c mv of
                      Output o nxtCmd ->
                        case parseResponse o of
                          Just Wall -> pure Nothing
                          Just AtTarget -> k $ Just (acc + 1, (coord, nxtCmd))
                          Just Moved -> pure $ Just (coord, nxtCmd)
                          _ -> pure Nothing
                      _ -> pure Nothing
                  | (mv, coord) <- [ (1, (x, y+1))
                                   , (2, (x, y-1))
                                   , (3, (x-1, y))
                                   , (4, (x+1, y))
                                   ]
                  , not $ S.member coord visited
                  ]
      go _ = pure []

  nextLocs <- M.toList . M.fromList . catMaybes . concat <$> traverse go locs
  let visited' = foldr (S.insert . fst) visited locs
  pure $ findTarget visited' nextLocs (acc + 1)


fillChamber :: S.Set (Int, Int) -> [Loc] -> Int -> Maybe Int
fillChamber _ [] acc = Just $ acc - 1
fillChamber visited locs !acc =
  let go ((x, y), Input c) =
        if S.member (x, y) visited
           then []
           else [ case c mv of
                    Output o nxtCmd ->
                      case parseResponse o of
                        Just Wall -> Nothing
                        Just AtTarget -> Nothing
                        Just Moved -> Just (coord, nxtCmd)
                        _ -> Nothing
                    _ -> Nothing
                | (mv, coord) <- [ (1, (x, y+1))
                                 , (2, (x, y-1))
                                 , (3, (x-1, y))
                                 , (4, (x+1, y))
                                 ]
                , not $ S.member coord visited
                ]
      go _ = []

      nextLocs = M.toList . M.fromList . catMaybes $ concatMap go locs
      visited' = foldr (S.insert . fst) visited locs
   in fillChamber visited' nextLocs (acc + 1)
