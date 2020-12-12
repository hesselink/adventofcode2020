import Control.Arrow (first, second)

main :: IO ()
main = do
  f <- readFile "input/12"
  let is = map parseInstr . lines $ f
      final = foldl (flip runInstr) initialState is
  print (manhattanDistance (pos final))
  let final2 = foldl (flip runInstr2) initialState is
  print (manhattanDistance (pos final2))

data Instr = Instr Op Int
  deriving (Show, Eq)

data Op = N | S | E | W | L | R | F
  deriving (Show, Read, Eq)

parseInstr :: String -> Instr
parseInstr (c:rs) = Instr (read [c]) (read rs)
parseInstr [] = error "Failed to parse instr"

data State = State 
  { pos :: (Int, Int)
  , dir :: Int -- East == 0, South == 90
  , waypoint :: (Int, Int)
  } deriving (Show, Eq)

initialState :: State
initialState = State (0,0) 0 (10,-1)

runInstr :: Instr -> State -> State
runInstr (Instr o n) s = case o of
  N -> s { pos = second (subtract n) (pos s) }
  S -> s { pos = second (+ n) (pos s) }
  E -> s { pos = first (+ n) (pos s) }
  W -> s { pos = first (subtract n) (pos s) }
  L -> s { dir = (dir s - n + 360) `mod` 360 }
  R -> s { dir = (dir s + n) `mod` 360 }
  F -> s { pos = translate (dir s) n (pos s) }

translate :: Int -> Int -> (Int, Int) -> (Int, Int)
translate d n (x, y) = (x + n * round (cos (pi * fromIntegral d / 180)), y + n * round (sin (pi * fromIntegral d / 180)))

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y

runInstr2 :: Instr -> State -> State
runInstr2 (Instr o n) s = case o of
  N -> s { waypoint = second (subtract n) (waypoint s) }
  S -> s { waypoint = second (+ n) (waypoint s) }
  E -> s { waypoint = first (+ n) (waypoint s) }
  W -> s { waypoint = first (subtract n) (waypoint s) }
  L -> s { waypoint = rotate ((360 - n) `mod` 360) (waypoint s) }
  R -> s { waypoint = rotate n (waypoint s) }
  F -> s { pos = pos s `plus` (n `times` waypoint s) }
  where
    plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    times m (x, y) = (m * x, m * y)

rotate :: Int -> (Int, Int) -> (Int, Int)
rotate 90 (x, y) = (-y, x)
rotate 180 (x, y) = (-x, -y)
rotate 270 (x, y) = (y, -x)
rotate _ _ = error "Illegal rotation"
