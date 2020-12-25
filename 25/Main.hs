import Data.List
import Data.Maybe

main :: IO ()
main = do
  let cardLoopSize = loopSize cardPubKey
      doorLoopSize = loopSize doorPubKey
      encryptionKey = iterate (step cardPubKey) 1 !! doorLoopSize
      encryptionKey2 = iterate (step doorPubKey) 1 !! cardLoopSize
  print (cardLoopSize, doorLoopSize)
  print (encryptionKey, encryptionKey2)

cardPubKey, doorPubKey :: PubKey
cardPubKey = 13316116
doorPubKey = 13651422

type Value = Int
type Subject = Int
type PubKey = Int

step :: Subject -> Value -> Value
step s v = (v * s) `mod` 20201227

loopSize :: PubKey -> Int
loopSize k = fromJust . elemIndex k . iterate (step 7) $ 1
