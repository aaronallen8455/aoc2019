{-# LANGUAGE OverloadedStrings #-}
module Day.Six
  ( daySixA
  , daySixB
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Bifunctor (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Sum(..))
import           Data.Tuple (swap)

daySixA :: BS.ByteString -> BS.ByteString
daySixA inp = fromMaybe "invalid input" $ do
  tree <- M.fromListWith (++) . map (second pure) <$> parseInput inp
  res <- countNode tree (Sum 0) "COM"
  Just . BS8.pack . show $ getSum res

daySixB :: BS.ByteString -> BS.ByteString
daySixB inp = fromMaybe "invalid input" $ do
  tree <- M.fromList . map swap <$> parseInput inp
  res <- minPath M.empty tree (Just "YOU") (Just "SAN") 0
  Just . BS8.pack $ show res

parseInput :: BS.ByteString -> Maybe [(BS.ByteString, BS.ByteString)]
parseInput =  traverse parsePair . BS8.lines

parsePair :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parsePair bs = case BS8.split ')' bs of
                 [a, b] -> Just (a, b)
                 _ -> Nothing

countNode :: M.Map BS.ByteString [BS.ByteString] -> Sum Int -> BS.ByteString -> Maybe (Sum Int)
countNode m acc key =
  Just acc <> (foldMap (countNode m (Sum 1 <> acc)) =<< M.lookup key m)

minPath :: M.Map BS.ByteString Int -> M.Map BS.ByteString BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> Int -> Maybe Int
minPath _ _ Nothing Nothing _ = Nothing
minPath visited m a b x
    = x * 2 - 2 <$ guard (a == b)
  <|> subtract 2 . (+ x) <$> (av <|> bv)
  <|> minPath visited' m ap bp (x + 1)
  where
    av = flip M.lookup visited =<< a
    bv = flip M.lookup visited =<< b
    ap = flip M.lookup m =<< a
    bp = flip M.lookup m =<< b
    visited' = mappend visited . fromMaybe mempty
             $ (M.singleton <$> a <*> Just x)
            <> (M.singleton <$> b <*> Just x)

