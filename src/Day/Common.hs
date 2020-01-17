{-# LANGUAGE OverloadedStrings #-}
module Day.Common
  ( commaSep
  , readInt
  , chunks
  ) where

import           Control.Monad (guard)
import qualified Data.ByteString.Char8 as BS

-- | Break comma seperated values
commaSep :: BS.ByteString -> [BS.ByteString]
commaSep = BS.splitWith (== ',')

readInt :: BS.ByteString -> Maybe Int
readInt x = do
  (i, r) <- BS.readInt x
  guard $ r == "" || r == "\n"
  pure i

chunks :: Int -> BS.ByteString -> [BS.ByteString]
chunks n bs = case BS.splitAt n bs of
                (c, "") -> [c]
                (c, r) -> c : chunks n r
