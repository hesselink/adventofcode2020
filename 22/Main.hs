import Control.Arrow ((***))
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State

main :: IO ()
main = do
  f <- readFile "input/22"
  let initialDecks = parseInput f
      finalDecks = play initialDecks
      result = winningScore finalDecks
  print result
  let finalDecks2 = play2 initialDecks
  let result2 = winningScore finalDecks2
  print result2

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

type Seen = Set Decks

step2 :: Decks -> Decks
step2 s =
  let (c1:rs1) = deck1 s
      (c2:rs2) = deck2 s
  in if c1 <= length rs1 && c2 <= length rs2
     then
       let ss = play2 (Decks (take c1 rs1) (take c2 rs2))
       in if null (deck1 ss)
          then Decks
            { deck1 = rs1
            , deck2 = rs2 ++ [c2, c1]
            }
          else Decks
            { deck1 = rs1 ++ [c1, c2]
            , deck2 = rs2
            }
     else if c1 > c2
          then Decks
            { deck1 = rs1 ++ [c1, c2]
            , deck2 = rs2
            }
          else Decks
            { deck1 = rs1
            , deck2 = rs2 ++ [c2, c1]
            }

play2 :: Decks -> Decks
play2 = flip State.evalState Set.empty . go
  where
    go :: Decks -> State Seen Decks
    go s = do
      seen <- State.get
      if s `Set.member` seen
      then do
        return $ Decks (deck1 s) []
      else do
        State.put (Set.insert s seen)
        let s' = step2 s
        if null (deck1 s') || null (deck2 s')
        then return s'
        else go s'
