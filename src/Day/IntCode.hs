module Day.IntCode
  ( runIntCodeProgram
  , parseInput
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

import           Debug.Trace

parseInput :: BS.ByteString -> Maybe (M.IntMap Int)
parseInput = fmap (M.fromList . zip [0..]) . traverse readInt . commaSep

runIntCodeProgram :: [Int] -> M.IntMap Int -> Maybe [Int]
runIntCodeProgram inp prgm = go inp 0 0 prgm where
  go :: [Int] -> Int -> Int -> M.IntMap Int -> Maybe [Int]
  go inp base i m =
    case parseOp $ m M.! i of
      1:p -> trace "case1" $ go inp base (i + 4) =<< doOp (+) p
      2:p -> trace "case2" $ go inp base (i + 4) =<< doOp (*) p
      3:p:_ -> trace "case3" $ case3 p inp
      4:p:_ -> trace "case4" $ (:) <$> getParam p (i + 1) <*> go inp base (i + 2) m
      5:p -> trace "case5" $ jump (/= 0) p
      6:p -> trace "case6" $ jump (== 0) p
      7:p -> trace "case7" $ go inp base (i + 4) =<< ifStore (<) p
      8:p -> trace "case8" $ go inp base (i + 4) =<< ifStore (==) p
      9:p:_ -> trace "case9" (\b -> go inp (base + b) (i + 2) m) =<< getParam p (i + 1)
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

        case3 p ~(x:xs) = traceShow x $
          let m' = M.insert
                    <$> insertParam p (i + 1)
                    <*> Just x
                    <*> Just m
           in go xs base (i + 2) =<< m'

        jump pred (p1 : p2 : _) = trace "in 6" $
          case pred <$> getParam p1 (i + 1) of
            Just True -> trace "true" $ flip (go inp base) m =<< getParam p2 (i + 2)
            _ -> trace "other" $ go inp base (i + 3) m

        ifStore cmp (p1 : p2 : p3 : _) =
          M.insert <$> insertParam p3 (i + 3)
                   <*> v
                   <*> Just m
          where
            v = fmap fromEnum $ cmp <$> getParam p1 (i + 1)
                                    <*> getParam p2 (i + 2)

        getParam 0 x = trace "0" memLookup =<< memLookup x
        getParam 1 x = trace "1" memLookup x
        getParam 2 x = trace "2" memLookup . (+ base) =<< memLookup x
        getParam _ _ = Nothing

        insertParam 0 = memLookup
        insertParam 2 = fmap (+ base) . memLookup
        insertparam _ = Nothing

        memLookup x = M.lookup x m <|> Just 0

-- need to check param code for write differently than read?

parseOp :: Int -> [Int]
parseOp inp = op : params where
  (inp', op) = inp `divMod` 100
  params = unfoldr (Just . swap . (`divMod` 10)) inp'

