{-# LANGUAGE OverloadedStrings #-}
module Day.Seven
  ( daySevenA
  , daySevenB
  ) where

import           Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.List (permutations)
import qualified Data.List.NonEmpty as NE

import           Day.IntCode (parseInput, runIntCodeProgram, runIntCodeProgramUnsafe)

import           Debug.Trace

daySevenA :: BS.ByteString -> BS.ByteString
daySevenA inp = maybe "invalid" (BS8.pack . show . maximum) . NE.nonEmpty $ do
  Just codes <- [parseInput inp]
  perms <- permutations [0..4]
  foldM (\a i -> concat $ runIntCodeProgram [i, a] codes ) 0 perms

daySevenB :: BS.ByteString -> BS.ByteString
daySevenB inp = maybe "invalid" (BS8.pack . show . maximum) . NE.nonEmpty $ do
  Just codes <- [parseInput inp]
  perms <- permutations [5..9]

  let r = foldr (\i a -> runIntCodeProgramUnsafe codes (i : a)) (0 : r) perms

  (x:_) <- [reverse r]
  pure x
