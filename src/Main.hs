module Main where

import           System.Environment (getArgs)

import           Day.One (dayOneA, dayOneB)
import           Day.Two (dayTwoA, dayTwoB)
import           Day.Three (dayThreeA)

main :: IO ()
main = do
  [day] <- getArgs
  interact $ case day of
    "1a" -> dayOneA
    "1b" -> dayOneB
    "2a" -> dayTwoA
    "2b" -> dayTwoB
    "3a" -> dayThreeA
    _ -> fail "day not found!"
