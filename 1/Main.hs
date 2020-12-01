main :: IO ()
main = do
  f <- readFile "input/1"
  let numbers = map read . lines $ f
      result = head [ x * y | x <- numbers, y <- numbers, x + y == (2020 :: Integer) ]
  print result
  let result2 = head [ x * y * z | x <- numbers, y <- numbers, z <- numbers, x + y + z == 2020 ]
  print result2
