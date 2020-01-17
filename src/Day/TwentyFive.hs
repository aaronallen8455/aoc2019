{-# LANGUAGE OverloadedStrings #-}
module Day.TwentyFive where

import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (fromMaybe)

import           Day.IntCode (parseInput, runIntCodeProgram)

dayTwentyFiveA :: BS8.ByteString -> BS8.ByteString
dayTwentyFiveA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  pure "test"
