import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (Array, UArray, bounds, listArray, range,
                                     (!))
import           Data.Function      ((&))

main :: IO ()
main = print (dp ! (1,1))

dp :: Array (Int,Int) Int
dp = listArray rng (map f (range rng))
  where
    f (y,x)
        | y == n = arr ! (y,x)
        | otherwise = arr ! (y,x) + max (dp ! (y+1,x)) (dp ! (y+1,x+1))

arr :: UArray (Int,Int) Int
arr = input
    & zipWith (\i x -> x <> replicate i 0) [n-1,n-2..]
    & concat
    & listArray rng

n :: Int
n = length (last input)

rng :: ((Int,Int),(Int,Int))
rng = ((1,1),(n,n))

input :: [[Int]]
input = map (words >>> map read) [
                  "75",
                 "95 64",
                "17 47 82",
               "18 35 87 10",
              "20 04 82 47 65",
             "19 01 23 75 03 34",
            "88 02 77 73 07 63 67",
           "99 65 04 28 06 16 70 92",
          "41 41 26 56 83 40 80 70 33",
         "41 48 72 33 47 32 37 16 94 29",
        "53 71 44 65 25 43 91 52 97 51 14",
       "70 11 33 28 77 73 17 78 39 68 17 57",
      "91 71 52 38 17 14 91 43 58 50 27 29 48",
     "63 66 04 68 89 53 67 30 73 16 69 87 40 31",
    "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
    ]
