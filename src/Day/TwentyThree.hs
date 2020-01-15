{-# LANGUAGE OverloadedStrings #-}
module Day.TwentyThree
  ( dayTwentyThreeA
  , dayTwentyThreeB
  ) where

import           Control.Arrow (first, second, (&&&))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IM
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import           Data.Sequence hiding (partition, zip, null, length)

import           Day.IntCode (Command(..), parseInput, runIntCodeProgram'')

dayTwentyThreeA :: BS8.ByteString -> BS8.ByteString
dayTwentyThreeA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  let nodes = [ (i, h i)
              | i <- [0..49]
              , Input h <- [runIntCodeProgram'' codes]
              ]
      queues = IM.fromList $ zip [0..49] (repeat mempty)
  r <- flip evalState (queues, mempty) $ runNetwork nodes `runContT` pure
  pure . BS8.pack $ show r

dayTwentyThreeB :: BS8.ByteString -> BS8.ByteString
dayTwentyThreeB inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  let nodes = [ (i, h i)
              | i <- [0..49]
              , Input h <- [runIntCodeProgram'' codes]
              ]
      queues = IM.insert 255 mempty
             . IM.fromList $ zip [0..49] (repeat mempty)
  r <- flip evalState (queues, mempty) $ runNetworkNAT 0 nodes
  pure . BS8.pack $ show r

runNetwork :: [(Int, Command)]
           -> ContT r (State ( IM.IntMap (Seq Int) -- queued packets
                             , IM.IntMap (Int, Maybe Int) -- partially formed packets
                             )
                      )
                      (Maybe Int)
runNetwork nodes = callCC $ \k -> do
  let nodes' = uncurry (++) $ partition (isOutput . snd) nodes
      isOutput (Output _ _) = True
      isOutput _ = False
      go (i, cmd) =
        case cmd of
          Output o cmd' -> do
            mbO <- lift $ gets (IM.lookup i . snd)
            case mbO of
              Just (255, Just _) -> k $ Just o
              Just (to, Just x) ->
                lift . modify' $ \(_1, _2) ->
                  ( IM.update (\q -> Just $ q :|> x :|> o) to _1
                  , IM.delete i _2
                  )
              Just (to, Nothing) ->
                lift . modify' . second $ IM.insert i (to, Just o)
              Nothing ->
                lift . modify' . second $ IM.insert i (o, Nothing)
            pure (i, cmd')
          Input handler -> do
            q <- lift $ gets ((IM.! i) . fst)
            case q of
              x :<| q' -> lift $ do
                modify' . first $ IM.insert i q'
                pure (i, handler x)
              _ -> pure (i, handler (-1))
          x -> k Nothing *> pure (i, x)

  traverse go nodes' >>= runNetwork

runNetworkNAT :: Int
              -> [(Int, Command)]
              -> State ( IM.IntMap (Seq Int) -- queued packets
                       , IM.IntMap (Int, Maybe Int) -- partially formed packets
                       )
                       (Maybe Int)
runNetworkNAT 2 nodes = do
  nat <- gets ((IM.! 255) . fst)
  case nat of
    _ :<| y1 :<| (_ :|> _ :|> y2) | y1 == y2 -> pure $ Just y1
    _ :|> x :|> y -> do
      modify' . first $ IM.insert 255 (x :<| y :<| Empty)
                      . IM.insert 0 (x :<| y :<| Empty)
      runNetworkNAT 0 nodes
    _ -> pure Nothing

runNetworkNAT idleCount nodes = do
  qs <- fst <$> get
  let (nodes', numOutputs) = uncurry (++) &&& length . fst
                           $ partition (isOutput . snd) nodes
      isOutput (Output _ _) = True
      isOutput _ = False
      idleCount' = if numOutputs == 0 && all null (IM.delete 255 qs)
                      then idleCount + 1
                      else 0
      go (i, cmd) =
        case cmd of
          Output o cmd' -> do
            mbO <- gets (IM.lookup i . snd)
            case mbO of
              Just (to, Just x) ->
                modify' $ \(_1, _2) ->
                  ( IM.update (\q -> Just $ q :|> x :|> o) to _1
                  , IM.delete i _2
                  )
              Just (to, Nothing) ->
                modify' . second $ IM.insert i (to, Just o)
              Nothing ->
                modify' . second $ IM.insert i (o, Nothing)
            pure (i, cmd')
          Input handler -> do
            q <- gets ((IM.! i) . fst)
            case q of
              x :<| q' -> do
                modify' . first $ IM.insert i q'
                pure (i, handler x)
              _ -> pure (i, handler (-1))
          x -> pure (i, x)

  traverse go nodes' >>= runNetworkNAT idleCount'
