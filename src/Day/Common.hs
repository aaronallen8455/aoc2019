module Day.Common
  ( commaSep
  ) where

import           Data.Function (on)
import           Data.List (groupBy)

-- | Break comma seperated values
commaSep :: String -> [String]
commaSep = filter (/= ",") . groupBy ((==) `on` (== ','))

