{-# LANGUAGE NumericUnderscores #-}
import Control.Arrow (first)
import Control.Monad
import Control.Monad.State.Strict (runState, state)
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import Prelude hiding (pi)
import System.Environment
import qualified Control.Monad.State.Strict as S
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq

main :: IO ()
main = do
  let finalState = iterate step initialState !! 100
      result = orderAfter1 finalState
  print result
  [arg] <- getArgs
  let finalState2 = iterate step initialState2 !! read arg
      (x,y) = findTwoAfter1 $ finalState2
  print (x*y)

initialState :: State2
initialState = mkState [3,6,2,9,8,1,7,5,4]

calculateDestination :: Foldable t => Int -> Int -> t Int -> Int
calculateDestination num cur excluded =
  let next = (cur - 2) `mod` num + 1
  in if next `elem` excluded
     then calculateDestination num next excluded
     else next

orderAfter1 :: State2 -> Int
orderAfter1 s = if current s == 1
  then read . concatMap show . fst . takeZ 8 $ s
  else orderAfter1 (rotate s)

findTwoAfter1 :: State2 -> (Int, Int)
findTwoAfter1 s =
  case Map.lookup 1 (pendingInserts s) of
    Just (x:<|y:<|_) ->
      case Map.lookup x (pendingInserts s) of
        Just (y':<|_) -> (x, y')
        _ -> (x, y)
    _ ->
      let [x, y] = F.toList . Seq.take 2 . Seq.drop 1 . Seq.dropWhileL (/= 1) . after $ s
      in case Map.lookup x (pendingInserts s) of
           Just (y':<|_) -> (x, y')
           _ -> (x, y)

data State2 = State2
  { current :: Int
  , after :: Seq Int
  , pendingInserts :: HashMap Int (Seq Int)
  , size :: Int
  } deriving (Show, Eq)

initialState2 :: State2
initialState2 = mkState $ [3,6,2,9,8,1,7,5,4] ++ [10..1_000_000]

mkState :: [Int] -> State2
mkState [] = error "empty list"
mkState (x:xs) = State2
  { current = x
  , after = Seq.fromList xs
  , pendingInserts = mempty
  , size = length (x:xs)
  }

rotate :: State2 -> State2
rotate (State2 _ Empty _ _) = error "empty list"
rotate (State2 c (x:<|xs) pi sz) =
  let (ys, pi') = lookupPending x pi
  in State2 x (ys <> xs |> c) pi' sz

lookupPending :: Int -> HashMap Int (Seq Int) -> (Seq Int, HashMap Int (Seq Int))
lookupPending x pi =
  case Map.lookup x pi of
    Nothing -> (mempty, pi)
    Just ys -> (ys, Map.delete x pi)

step :: State2 -> State2
step s =
  let (removed, State2 c (x:<|xs) pi sz) = takeZ 3 s
      dest = calculateDestination sz c removed
      pi' = Map.insertWith (flip (<>)) dest removed pi
      (ys, pi'') = lookupPending x pi'
      -- (b2, a2) = (insertBefore dest (reverse removed) b, insertAfter dest removed a)
  in State2
       { current = x
       , after = ys <> xs |> c
       , pendingInserts = pi''
       , size = sz
       }

takeZ :: Int -> State2 -> (Seq Int, State2)
takeZ n = first Seq.fromList . runState (replicateM n removeOne)

removeOne :: S.State State2 Int
removeOne = state $ \(State2 c (x:<|xs) pi sz) ->
  let (ys, pi') = lookupPending x pi
  in (x, State2 c (ys <> xs) pi' sz)
