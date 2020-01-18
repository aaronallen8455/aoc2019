{-# LANGUAGE OverloadedStrings #-}
module Day.TwentyFive
  ( dayTwentyFiveA
  ) where

import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable (asum)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Traversable (for)

import           Day.IntCode (Command(..), parseInput, runIntCodeProgram'')

dayTwentyFiveA :: BS8.ByteString -> BS8.ByteString
dayTwentyFiveA inp = fromMaybe "invalid program" $ do
  codes <- parseInput inp
  let c = execCommands (runIntCodeProgram'' codes) collectItemsCmds
  (r, _) <- findWeight c
  pure $ BS8.pack r

-- UI for exploring the ship
explore :: IO ()
explore = do
  Just inp <- parseInput <$> BS8.readFile "input/daytwentyfive.txt"
  let initCmd = runIntCodeProgram'' inp
      runCmd mbI cmd =
        case cmd of
          Input h | Just [] <- mbI -> do
                      runCmd Nothing . h $ fromEnum '\n'
                  | Just (x:xs) <- mbI -> do
                      runCmd (Just xs) $ h x
                  | otherwise -> do
                      i <- getLine
                      case i of
                        "find the weight" ->
                           case findWeight (Input h) of
                             Nothing -> putStrLn "I have failed you."
                             Just (msg, nxt) -> do
                               putStr msg
                               runCmd Nothing nxt
                        "collect items" ->
                          runCmd Nothing $ execCommands (Input h) collectItemsCmds
                        _ -> runCmd (Just $ fromEnum <$> i) $ Input h
          Output o nxt -> do
            putStr $ [toEnum o]
            runCmd Nothing nxt
          Halt -> pure ()

  runCmd Nothing initCmd

items :: S.Set String
items = S.fromList
  [ "shell"
  , "klein bottle"
  , "tambourine"
  , "weather machine"
  , "antenna"
  , "spool of cat6"
  , "mug"
  , "cake"
  ]

findWeight :: Command -> Maybe (String, Command)
findWeight cmd@(Input _) = evalState (goIn cmd items "east\n") S.empty
  where
    goOut (Output o nxt) is out = goOut nxt is (toEnum o : out)
    goOut c@(Input _) is out
      | elem "pord" ws = goIn c is "east\n"
      | elem "rethgil" ws = fmap asum .
        for (S.toList is) $ \i -> do
          let is' = S.delete i is
          visited <- gets $ S.member is'
          modify' $ S.insert is'
          if visited
             then pure Nothing
             else goIn c is' ("drop " ++ i ++ "\n")
      | elem "reivaeh" ws = do
        pure Nothing
      | otherwise = pure $ Just (out, c)
      where ws = words out
    goOut Halt _ out = pure $ Just (reverse out, Halt)
    goIn (Input h) is (x:xs) = goIn (h $ fromEnum x) is xs
    goIn c@(Output _ _) is _ = goOut c is ""
    goIn _ _ _ = pure Nothing
findWeight _ = Nothing

execCommands :: Command -> [String] -> Command
execCommands c [] = c
execCommands (Output _ o) c = execCommands o c
execCommands n@(Input _) ([] : c) = execCommands n c
execCommands (Input h) ((x:xs):c) = execCommands (h (fromEnum x)) (xs : c)
execCommands Halt _ = Halt

collectItemsCmds :: [String]
collectItemsCmds =
  [ "east\n"
  , "take antenna\n"
  , "west\n"
  , "north\n"
  , "take weather machine\n"
  , "north\n"
  , "take klein bottle\n"
  , "east\n"
  , "take spool of cat6\n"
  , "east\n"
  , "south\n"
  , "take mug\n"
  , "north\n"
  , "north\n"
  , "east\n"
  , "south\n"
  , "take shell\n"
  , "north\n"
  , "north\n"
  , "north\n"
  , "take tambourine\n"
  , "south\n"
  , "south\n"
  , "west\n"
  , "west\n"
  , "north\n"
  , "take cake\n"
  , "south\n"
  , "east\n"
  , "south\n"
  , "west\n"
  , "south\n"
  , "south\n"
  ]
