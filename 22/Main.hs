import Control.Arrow ((***))
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Strict (State, StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import Debug.Trace
import Data.Hashable (Hashable, hash, hashWithSalt)

main :: IO ()
main = do
  f <- readFile "input/22"
  let initialDecks = parseInput f
      finalDecks = play initialDecks
      result = winningScore finalDecks
  -- print result
  let finalDecks2 = State.evalState (play2 initialDecks) Set.empty
  let result2 = winningScore finalDecks2
  --print State.<=< State.execStateT (play2 initialDecks) Set.empty
  print result2

getS = do
  f <- readFile "input/22"
  let initialDecks = parseInput f
  return initialDecks

data Decks = Decks
  { deck1 :: Deck
  , deck2 :: Deck
  } deriving (Show, Eq, Ord)

type Deck = [Int]

parseInput :: String -> Decks
parseInput = uncurry Decks . (toDeck *** (toDeck . tail)) . break (== "") . lines
  where
    toDeck = map read . tail

step :: Decks -> Decks
step s =
  let (c1:rs1) = deck1 s
      (c2:rs2) = deck2 s
  in if c1 > c2
     then Decks
       { deck1 = rs1 ++ [c1, c2]
       , deck2 = rs2
       }
     else Decks
       { deck1 = rs1
       , deck2 = rs2 ++ [c2, c1]
       }

play :: Decks -> Decks
play s =
  let s' = step s
  in if null (deck1 s') || null (deck2 s')
     then s'
     else play s'

winningScore :: Decks -> Int
winningScore s =
  if null (deck1 s)
  then score (deck2 s)
  else score (deck1 s)

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse

type Seen = Set Int -- hash of Decks

instance Hashable Decks where
  hashWithSalt s d = hashWithSalt s (deck1 d, deck2 d)

step2 :: Decks -> State Seen Decks
step2 s =
  let (c1:rs1) = deck1 s
      (c2:rs2) = deck2 s
  in if c1 <= length rs1 && c2 <= length rs2
     then do
       -- liftIO $ putStrLn "Recursing"
       let ss = State.evalState (play2 (Decks rs1 rs2)) Set.empty
       -- liftIO $ putStrLn "Done with recursion"
       return $
         if null (deck1 ss)
         then Decks
           { deck1 = rs1
           , deck2 = rs2 ++ [c2, c1]
           }
         else Decks
           { deck1 = rs1 ++ [c1, c2]
           , deck2 = rs2
           }
     else return $
            if c1 > c2
            then Decks
              { deck1 = rs1 ++ [c1, c2]
              , deck2 = rs2
              }
            else Decks
              { deck1 = rs1
              , deck2 = rs2 ++ [c2, c1]
              }

play2 :: Decks -> State Seen Decks
play2 s = do
  -- liftIO $ putStrLn "Playing"
  -- liftIO $ print s
  seen <- State.get
  if hash s `Set.member` seen
  then do
    -- liftIO $ putStrLn "Already seen, p1 wins"
    -- liftIO $ print (seen, s)
    return $ Decks (deck1 s) []
  else do
    -- liftIO $ putStrLn "Inserting state"
    -- liftIO $ print s
    State.put (Set.insert (hash s) seen)
    s' <- step2 s
    if null (deck1 s') || null (deck2 s')
    then return s'
    else play2 s'
