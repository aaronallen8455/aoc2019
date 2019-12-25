{-# LANGUAGE OverloadedStrings #-}
module Day.Nine
  ( dayNineA
  , dayNineB
  ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (fromMaybe)

import           Day.IntCode (parseInput, runIntCodeProgram)

dayNineA :: BS8.ByteString -> BS8.ByteString
dayNineA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  res <- runIntCodeProgram [1] codes
  pure . BS8.pack $ show res

dayNineB :: BS8.ByteString -> BS8.ByteString
dayNineB inp = fromMaybe "invalid program" $ do
  res <- runIntCodeProgram [2] =<< parseInput inp
  pure . BS8.pack $ show res
