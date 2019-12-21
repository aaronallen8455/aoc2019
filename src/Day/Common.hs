{-# LANGUAGE OverloadedStrings #-}
module Day.Common
  ( commaSep
  , readInt
  ) where

import           Control.Monad (guard)
import qualified Data.ByteString.Char8 as BS
import           Data.Function (on)
import           Data.List (groupBy)

-- | Break comma seperated values
commaSep :: BS.ByteString -> [BS.ByteString]
commaSep = BS.splitWith (== ',')

readInt :: BS.ByteString -> Maybe Int
readInt x = do
  (i, r) <- BS.readInt x
  guard $ r == "" || r == "\n"
  pure i
