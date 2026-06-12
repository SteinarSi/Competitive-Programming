import           Data.Array (Array, listArray, range, (!))
import           Data.Bool  (bool)

main :: IO ()
main = print (dp ! (0,0))

dp :: Array (Int,Int) Int
dp = listArray rng (map f (range rng))
  where
    limit = 20
    rng = ((0,0),(limit,limit))
    f (x,y)
        | (x,y) == snd rng = 1
        | otherwise = bool 0 (dp ! (x+1,y)) (x < limit) + bool 0 (dp ! (x,y+1)) (y < limit)
