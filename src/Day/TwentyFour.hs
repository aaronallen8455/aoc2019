{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Day.TwentyFour where

import           Control.Comonad
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (fromMaybe)
import qualified Data.IntSet as IS

dayTwentyFourA :: BS8.ByteString -> BS8.ByteString
dayTwentyFourA inp = fromMaybe "invalid input" $ do
  eris <- parseEris inp
  let scores = score <$> iterate (extend step) eris
      go e nxt s =
        if IS.member e s
           then Just e
           else nxt $ IS.insert e s
  r <- foldr go (const Nothing) scores IS.empty
  pure . BS8.pack $ show r

data Eris a =
  Eris
    { focus :: (Int, Int)
    , grid :: Array (Int, Int) a
    } deriving Functor

instance Comonad Eris where
  extract = (!) <$> grid <*> focus
  duplicate e =
    e { grid = listArray (bounds $ grid e)
                         [ Eris i (grid e)
                         | i <- indices (grid e)
                         ]
      }

step :: Eris Bool -> Bool
step e
  | focusedBug && numBugs /= 1 = False
  | not focusedBug && (numBugs == 1 || numBugs == 2) = True
  | otherwise = focusedBug
  where
    (_, (bx, by)) = bounds $ grid e
    (fx, fy) = focus e
    adj = [ (x, y)
          | (x, y) <- [(fx-1, fy), (fx+1, fy), (fx, fy-1), (fx, fy+1)]
          , x >= 0 && x <= bx , y >= 0 && y <= by
          ]
    numBugs = length . filter id $ map (grid e !) adj
    focusedBug = grid e ! focus e

score :: Eris Bool -> Int
score = sum . map fst . filter snd . zip powersOfTwo . elems . grid

powersOfTwo :: [Int]
powersOfTwo = (2 ^) <$> [(0 :: Int) ..]

parseEris :: BS8.ByteString -> Maybe (Eris Bool)
parseEris bs = do
  let rows = BS8.lines bs
      nrows = length rows
  h : _ <- Just rows
  let ncols = BS8.length h
      g = listArray ((0, 0), (ncols - 1, nrows - 1))
        . map (== '#') $ concatMap BS8.unpack rows
  Just $ Eris (0, 0) g

