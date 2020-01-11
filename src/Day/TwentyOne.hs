{-# LANGUAGE OverloadedStrings #-}
module Day.TwentyOne
  ( dayTwentyOneA
  , dayTwentyOneB
  ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (fromMaybe)

import Day.IntCode (parseInput, runIntCodeProgram)

dayTwentyOneA :: BS8.ByteString -> BS8.ByteString
dayTwentyOneA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  output : _ <- reverse <$> runIntCodeProgram (fromEnum <$> springScript) codes
  pure . BS8.pack $ show output

dayTwentyOneB :: BS8.ByteString -> BS8.ByteString
dayTwentyOneB inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  output : _ <- reverse <$> runIntCodeProgram (fromEnum <$> springScriptRun) codes
  pure . BS8.pack $ show output

springScript :: String
springScript
   = "NOT C T\n"
  <> "NOT A J\n"
  <> "OR T J\n" -- jump if 1 or 3 are empty
  <> "AND D J\n" -- but only if 4 is not empty
  <> "WALK\n"

springScriptRun :: String
springScriptRun
   = "NOT C T\n"
  <> "NOT A J\n"
  <> "OR T J\n" -- jump if 1 or 3 are empty
  <> "NOT B T\n"
  <> "OR T J\n" -- also jump if 2 is empty
  <> "AND D J\n" -- but only if 4 is not empty
  <> "NOT E T\n"
  <> "AND H T\n" -- if 5 is empty and 8 is not
  <> "OR E T\n" -- or if 5 is not empty
  <> "AND T J\n" -- then still ok to jump
  <> "RUN\n"

