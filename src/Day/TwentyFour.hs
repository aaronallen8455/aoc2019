{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Day.TwentyFour
  ( dayTwentyFourA
  , dayTwentyFourB
  ) where

import           Control.Comonad
import           Data.Array
import qualified Data.ByteString.Char8 as BS8
import           Data.List (transpose)
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

dayTwentyFourB :: BS8.ByteString -> BS8.ByteString
dayTwentyFourB inp = fromMaybe "invalid input" $ do
  eris <- parseEris inp
  let emptyEris = Eris (focus eris) (False <$ grid eris)
      zipper = Zipper [] eris []
  z : _ <- Just . drop 200 $ iterate (extend zipperStep . pad emptyEris) zipper
  let r = zipperBugCount z
  pure . BS8.pack $ show r

data Eris a =
  Eris
    { focus :: (Int, Int)
    , grid :: Array (Int, Int) a
    } deriving (Functor, Show)

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
        . map (== '#') . concat . transpose $ map BS8.unpack rows
  Just $ Eris (0, 0) g

data Zipper a =
  Zipper
    { zOuter :: [a]
    , zFocus :: a
    , zInner :: [a]
    } deriving (Functor, Show)

pad :: a -> Zipper a -> Zipper a
pad a (Zipper o f i) = Zipper (o ++ [a]) f (i ++ [a])

instance Comonad Zipper where
  extract = zFocus
  duplicate s =
    Zipper { zOuter = goLeft s
           , zFocus = s
           , zInner = goRight s
           }
    where
      goLeft (Zipper [] _ _) = []
      goLeft (Zipper (l : ls) a r) = let z = Zipper ls l (a : r) in z : goLeft z
      goRight (Zipper _ _ []) = []
      goRight (Zipper l a (r : rs)) = let z = Zipper (a : l) r rs in z : goRight z

data Direction
  = U
  | D
  | L
  | R

zipperStep :: Zipper (Eris Bool) -> Eris Bool
zipperStep (Zipper outer e inner) = extend (dimensionalStep outer inner) e

dimensionalStep :: [Eris Bool] -> [Eris Bool] -> Eris Bool -> Bool
dimensionalStep outer inner e
  | fx == mid && fy == mid = False
  | focusedBug && numBugs /= 1 = False
  | not focusedBug && numBugs `elem` [1, 2] = True
  | otherwise = focusedBug
  where
    focusedBug = grid e ! focus e
    (_, (b, _)) = bounds $ grid e
    mid = b `div` 2
    (fx, fy) = focus e
    oL = case outer of
           [] -> 0
           x : _ -> fromEnum $ grid x ! (mid - 1, mid)
    oR = case outer of
           [] -> 0
           x : _ -> fromEnum $ grid x ! (mid + 1, mid)
    oU = case outer of
           [] -> 0
           x : _ -> fromEnum $ grid x ! (mid, mid - 1)
    oD = case outer of
           [] -> 0
           x : _ -> fromEnum $ grid x ! (mid, mid + 1)
    iL = case inner of
           [] -> 0
           x : _ -> sum $ (fromEnum . (grid x !)) <$> (repeat 0 `zip` [0..b])
    iR = case inner of
           [] -> 0
           x : _ -> sum $ (fromEnum . (grid x !)) <$> (repeat b `zip` [0..b])
    iU = case inner of
           [] -> 0
           x : _ -> sum $ (fromEnum . (grid x !)) <$> ([0..b] `zip` repeat 0)
    iD = case inner of
           [] -> 0
           x : _ -> sum $ (fromEnum . (grid x !)) <$> ([0..b] `zip` repeat b)
    adj = [ bugs
          | (x, y, d) <- [(fx - 1, fy, L), (fx + 1, fy, R), (fx, fy - 1, U), (fx, fy + 1, D)]
          , let bugs =
                  case d of
                    L | x == mid && y == mid -> iR
                      | x < 0 -> oL
                      | otherwise -> fromEnum $ grid e ! (x, y)
                    R | x == mid && y == mid -> iL
                      | x > b -> oR
                      | otherwise -> fromEnum $ grid e ! (x, y)
                    U | x == mid && y == mid -> iD
                      | y < 0 -> oU
                      | otherwise -> fromEnum $ grid e ! (x, y)
                    D | x == mid && y == mid -> iU
                      | y > b -> oD
                      | otherwise -> fromEnum $ grid e ! (x, y)
          ]
    numBugs = sum adj

erisCount :: Eris Bool -> Int
erisCount = length . filter id . elems . grid

zipperBugCount :: Zipper (Eris Bool) -> Int
zipperBugCount  = do
  z <- fmap erisCount
  pure $ sum (zOuter z) + zFocus z + sum (zInner z)
