import           Control.Arrow ((>>>))
import           Data.Array    (listArray, range, (!))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,m] <- getContents <&> (words >>> map read)

    let dp = listArray ((0,0),(n,m)) (map f (range ((0,0),(n,m))))
        f (0,0) = 1
        f (0,g) | g <= m-n  = 1
                | otherwise = 0
        f (_,0) = 0
        f (b,g) = case compare b g of
            LT -> modulo (dp ! (b-1,g-1) + dp ! (b,g-1))
            EQ -> 1
            GT -> modulo (dp ! (b-1,g) + dp ! (b-1,g-1))

    print (dp ! (n,m))

modulo :: Int -> Int
modulo = (`mod` (10^9+7))
