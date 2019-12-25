module Main where

import qualified Data.ByteString.Char8 as BS8
import           System.Environment (getArgs)

import           Day.One (dayOneA, dayOneB)
import           Day.Two (dayTwoA, dayTwoB)
import           Day.Three (dayThreeA, dayThreeB)
import           Day.Four (dayFourA)
import           Day.Five (dayFiveA, dayFiveB)
import           Day.Six (daySixA, daySixB)
import           Day.Seven (daySevenA, daySevenB)
import           Day.Eight (dayEightA, dayEightB)
import           Day.Nine (dayNineA, dayNineB)
import           Day.Ten (dayTenA, dayTenB)
import           Day.Eleven (dayElevenA, dayElevenB)

main :: IO ()
main = do
  [day] <- getArgs
  BS8.interact $ case day of
    "1a" -> dayOneA
    "1b" -> dayOneB
    "2a" -> dayTwoA
    "2b" -> dayTwoB
    "3a" -> dayThreeA
    "3b" -> dayThreeB
    "4a" -> dayFourA
    "5a" -> dayFiveA
    "5b" -> dayFiveB
    "6a" -> daySixA
    "6b" -> daySixB
    "7a" -> daySevenA
    "7b" -> daySevenB
    "8a" -> dayEightA
    "8b" -> dayEightB
    "9a" -> dayNineA
    "9b" -> dayNineB
    "10a" -> dayTenA
    "10b" -> dayTenB
    "11a" -> dayElevenA
    "11b" -> dayElevenB
    _ -> fail "day not found!"
