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
import           Day.Twelve (dayTwelveA, dayTwelveB)
import           Day.Thirteen (dayThirteenA, dayThirteenB)
import           Day.Fourteen (dayFourteenA, dayFourteenB)
import           Day.Fifteen (dayFifteenA, dayFifteenB)
import           Day.Sixteen (daySixteenA, daySixteenB)
import           Day.Seventeen (daySeventeenA, daySeventeenB)
import           Day.Eighteen (dayEighteenA, dayEighteenB)
import           Day.Nineteen (dayNineteenA, dayNineteenB)

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
    "12a" -> dayTwelveA
    "12b" -> dayTwelveB
    "13a" -> dayThirteenA
    "13b" -> dayThirteenB
    "14a" -> dayFourteenA
    "14b" -> dayFourteenB
    "15a" -> dayFifteenA
    "15b" -> dayFifteenB
    "16a" -> daySixteenA
    "16b" -> daySixteenB
    "17a" -> daySeventeenA
    "17b" -> daySeventeenB
    "18a" -> dayEighteenA
    "18b" -> dayEighteenB
    "19a" -> dayNineteenA
    "19b" -> dayNineteenB
    _ -> fail "day not found!"
