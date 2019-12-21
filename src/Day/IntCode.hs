module Day.IntCode
  ( runIntCodeProgram
  , runIntCodeProgramUnsafe
  , parseInput
  ) where

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
runIntCodeProgram inp prgm = go inp 0 prgm where
  go inp@(~(x:xs)) i m =
    case parseOp $ m M.! i of
      1:p -> go inp (i + 4) =<< doOp (+) p
      2:p -> go inp (i + 4) =<< doOp (*) p
      3:_ -> go xs (i + 2) =<< case3
      4:p:_ -> (:) <$> getParam p (i + 1) <*> go inp (i + 2) m
      5:p -> jump (/= 0) p
      6:p -> jump (== 0) p
      7:p -> go inp (i + 4) =<< ifStore (<) p
      8:p -> go inp (i + 4) =<< ifStore (==) p
      99:_ -> Just []
      _ -> Nothing
      where
        doOp op (p1 : p2 : _) = M.insert
                    <$> M.lookup (i + 3) m
                    <*> (op <$> getParam p1 (i + 1)
                            <*> getParam p2 (i + 2)
                        )
                    <*> Just m

        case3 = M.insert
                  <$> M.lookup (i + 1) m
                  <*> Just x
                  <*> Just m

        jump pred (p1 : p2 : _) =
          case pred <$> getParam p1 (i + 1) of
            Just True -> flip (go inp) m =<< getParam p2 (i + 2)
            _ -> go inp (i + 3) m

        ifStore cmp (p1 : p2 : _) =
          M.insert <$> M.lookup (i + 3) m
                   <*> v
                   <*> Just m
          where
            v = fmap fromEnum $ cmp <$> getParam p1 (i + 1)
                                    <*> getParam p2 (i + 2)

        getParam 0 x = flip M.lookup m =<< M.lookup x m
        getParam 1 x = M.lookup x m
        getParam _ _ = Nothing

parseOp :: Int -> [Int]
parseOp inp = op : params where
  (inp', op) = inp `divMod` 100
  params = unfoldr (Just . swap . (`divMod` 10)) inp'


runIntCodeProgramUnsafe :: M.IntMap Int -> [Int] -> [Int]
runIntCodeProgramUnsafe = go 0 where
  go i m inp =
    case parseOp $ m M.! i of
      1:p -> go (i + 4) (doOp (+) p) inp
      2:p -> go (i + 4) (doOp (*) p) inp
      3:_ | x:xs <- inp -> go (i + 2) (case3 x) xs
      4:p:_ -> getParam p (i + 1) : go (i + 2) m inp
      5:p -> jump (/= 0) p
      6:p -> jump (== 0) p
      7:p -> go (i + 4) (ifStore (<) p) inp
      8:p -> go (i + 4) (ifStore (==) p) inp
      99:_ -> []
      p -> error "invalid program"
      where
        doOp op (p1 : p2 : _) =
          M.insert
            (m M.! (i + 3))
            (op (getParam p1 (i + 1)) (getParam p2 (i + 2)))
            m

        case3 x = M.insert
                  (m M.! (i + 1))
                  x
                  m

        jump pred (p1 : p2 : _) =
          if pred (getParam p1 (i + 1))
            then go (getParam p2 (i + 2)) m inp
            else go (i + 3) m inp

        ifStore cmp (p1 : p2 : _) =
          M.insert (m M.! (i + 3))
                   v
                   m
          where
            v = fromEnum $ cmp (getParam p1 (i + 1))
                               (getParam p2 (i + 2))

        getParam 0 x = m M.! (m M.! x)
        getParam 1 x = m M.! x
        getParam _ _ = error "invalid param"
