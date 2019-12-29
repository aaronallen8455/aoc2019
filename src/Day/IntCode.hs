module Day.IntCode
  ( runIntCodeProgram
  , runIntCodeProgram'
  , runIntCodeProgram''
  , parseInput
  , Command(..)
  ) where

import           Control.Applicative ((<|>))
import           Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isNumber)
import           Data.Function (on)
import qualified Data.IntMap as M
import           Data.List (groupBy, unfoldr)
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Tuple (swap)

import           Day.Common (commaSep, readInt)

parseInput :: BS.ByteString -> Maybe (M.IntMap Int)
parseInput = fmap (M.fromList . zip [0..]) . traverse readInt . commaSep

runIntCodeProgram :: [Int] -> M.IntMap Int -> Maybe [Int]
runIntCodeProgram inp prgm = go inp 0 0 prgm where
  go :: [Int] -> Int -> Int -> M.IntMap Int -> Maybe [Int]
  go inp base i m =
    case parseOp $ m M.! i of
      1:p -> go inp base (i + 4) =<< doOp (+) p
      2:p -> go inp base (i + 4) =<< doOp (*) p
      3:p:_ -> case3 p inp
      4:p:_ -> (:) <$> getParam p (i + 1) <*> go inp base (i + 2) m
      5:p -> jump (/= 0) p
      6:p -> jump (== 0) p
      7:p -> go inp base (i + 4) =<< ifStore (<) p
      8:p -> go inp base (i + 4) =<< ifStore (==) p
      9:p:_ -> (\b -> go inp (base + b) (i + 2) m) =<< getParam p (i + 1)
      99:_ -> Just []
      _ -> Nothing
      where
        doOp op (p1 : p2 : p3 : _) =
          M.insert
            <$> insertParam p3 (i + 3)
            <*> (op <$> getParam p1 (i + 1)
                    <*> getParam p2 (i + 2)
                )
            <*> Just m

        case3 p ~(x:xs) =
          let m' = M.insert
                    <$> insertParam p (i + 1)
                    <*> Just x
                    <*> Just m
           in go xs base (i + 2) =<< m'

        jump pred (p1 : p2 : _) =
          case pred <$> getParam p1 (i + 1) of
            Just True -> flip (go inp base) m =<< getParam p2 (i + 2)
            _ -> go inp base (i + 3) m

        ifStore cmp (p1 : p2 : p3 : _) =
          M.insert <$> insertParam p3 (i + 3)
                   <*> v
                   <*> Just m
          where
            v = fmap fromEnum $ cmp <$> getParam p1 (i + 1)
                                    <*> getParam p2 (i + 2)

        getParam 0 x = memLookup =<< memLookup x
        getParam 1 x = memLookup x
        getParam 2 x = memLookup . (+ base) =<< memLookup x
        getParam _ _ = Nothing

        insertParam 0 = memLookup
        insertParam 2 = fmap (+ base) . memLookup
        insertparam _ = Nothing

        memLookup x = M.lookup x m <|> Just 0

parseOp :: Int -> [Int]
parseOp inp = op : params where
  (inp', op) = inp `divMod` 100
  params = unfoldr (Just . swap . (`divMod` 10)) inp'

runIntCodeProgram' :: M.IntMap Int -> [Int] -> [Int]
runIntCodeProgram' prgm = go 0 0 prgm where
  go :: Int -> Int -> M.IntMap Int -> [Int] -> [Int]
  go base i m inp =
    case parseOp $ m M.! i of
      1:p -> go base (i + 4) (doOp (+) p) inp
      2:p -> go base (i + 4) (doOp (*) p) inp
      3:p:_ -> case3 p inp
      4:p:_ -> getParam p (i + 1) : go base (i + 2) m inp
      5:p -> jump (/= 0) p
      6:p -> jump (== 0) p
      7:p -> go base (i + 4) (ifStore (<) p) inp
      8:p -> go base (i + 4) (ifStore (==) p) inp
      9:p:_ -> (\b -> go (base + b) (i + 2) m inp) $ getParam p (i + 1)
      99:_ -> []
      _ -> error "invalid code"
      where
        doOp op (p1 : p2 : p3 : _) =
          M.insert
            (insertParam p3 (i + 3))
            (op (getParam p1 (i + 1))
                (getParam p2 (i + 2))
            )
            m

        case3 p ~(x:xs) =
          let m' = M.insert
                    (insertParam p (i + 1))
                    x
                    m
           in go base (i + 2) m' xs

        jump pred (p1 : p2 : _) =
          case pred (getParam p1 (i + 1)) of
            True -> go base (getParam p2 (i + 2)) m inp
            _ -> go base (i + 3) m inp

        ifStore cmp (p1 : p2 : p3 : _) =
          M.insert (insertParam p3 (i + 3))
                   v
                   m
          where
            v = fromEnum $ cmp (getParam p1 (i + 1))
                               (getParam p2 (i + 2))

        getParam 0 x = memLookup $ memLookup x
        getParam 1 x = memLookup x
        getParam 2 x = memLookup . (+ base) $ memLookup x
        getParam _ _ = error "bad param"

        insertParam 0 = memLookup
        insertParam 2 = (+ base) . memLookup
        insertparam _ = error "bad param"

        memLookup x = fromMaybe 0 $ M.lookup x m

data Command
  = Output Int Command
  | Input (Int -> Command)
  | Halt

runIntCodeProgram'' :: M.IntMap Int -> Command
runIntCodeProgram'' prgm = go 0 0 prgm where
  go :: Int -> Int -> M.IntMap Int -> Command
  go base i m =
    case parseOp $ m M.! i of
      1:p -> go base (i + 4) (doOp (+) p)
      2:p -> go base (i + 4) (doOp (*) p)
      3:p:_ -> case3 p
      4:p:_ -> Output (getParam p (i + 1)) (go base (i + 2) m)
      5:p -> jump (/= 0) p
      6:p -> jump (== 0) p
      7:p -> go base (i + 4) (ifStore (<) p)
      8:p -> go base (i + 4) (ifStore (==) p)
      9:p:_ -> (\b -> go (base + b) (i + 2) m) $ getParam p (i + 1)
      99:_ -> Halt
      _ -> error "invalid code"
      where
        doOp op (p1 : p2 : p3 : _) =
          M.insert
            (insertParam p3 (i + 3))
            (op (getParam p1 (i + 1))
                (getParam p2 (i + 2))
            )
            m

        case3 p = Input $ \x ->
          let m' = M.insert
                    (insertParam p (i + 1))
                    x
                    m
           in go base (i + 2) m'

        jump pred (p1 : p2 : _) =
          case pred (getParam p1 (i + 1)) of
            True -> go base (getParam p2 (i + 2)) m
            _ -> go base (i + 3) m

        ifStore cmp (p1 : p2 : p3 : _) =
          M.insert (insertParam p3 (i + 3))
                   v
                   m
          where
            v = fromEnum $ cmp (getParam p1 (i + 1))
                               (getParam p2 (i + 2))

        getParam 0 x = memLookup $ memLookup x
        getParam 1 x = memLookup x
        getParam 2 x = memLookup . (+ base) $ memLookup x
        getParam _ _ = error "bad param"

        insertParam 0 = memLookup
        insertParam 2 = (+ base) . memLookup
        insertparam _ = error "bad param"

        memLookup x = fromMaybe 0 $ M.lookup x m
