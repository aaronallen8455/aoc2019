{-# LANGUAGE OverloadedStrings #-}
module Day.Five
  ( dayFiveA
  , dayFiveB
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as M
import           Data.Maybe (fromMaybe)
import           Text.Read (readMaybe)

import           Day.Common (commaSep, readInt)
import           Day.IntCode (parseInput, runIntCodeProgram)

dayFiveA :: BS.ByteString -> BS.ByteString
dayFiveA = handleInput [1]

dayFiveB :: BS.ByteString -> BS.ByteString
dayFiveB = handleInput [5]

handleInput :: [Int] -> BS.ByteString -> BS.ByteString
handleInput inp bs
  = fromMaybe "invalid program!" . fmap (BS.pack . show . last)
  $ runIntCodeProgram inp =<< parseInput bs

