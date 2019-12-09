module Day.One
  ( dayOneA
  , dayOneB
  , calcFuel
  ) where

calcFuel :: Int -> Int
calcFuel = subtract 2 . (`div` 3)

dayOneA :: String -> String
dayOneA = show . sum
        . map (calcFuel . read)
        . lines

dayOneB :: String -> String
dayOneB = show . sum
        . map (sum . takeWhile (>0) . tail . iterate calcFuel . read)
        . lines
