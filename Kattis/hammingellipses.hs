import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (Array, UArray, listArray, range, (!))
import           Data.Functor       ((<&>))

main :: IO ()
main = do
    [q,n,d] <- getLine <&> (words >>> map read)
    f1 <- getLine
    f2 <- getLine

    let
        eq :: UArray Int Bool
        eq = listArray (1,n) (zipWith (==) f1 f2)

        dp :: Array (Int,Int) Int
        dp = listArray rng (map f (range rng))
          where
            rng = ((0,-2),(n,d))
            f (0,0) = 1
            f (0,_) = 0
            f (_,r) | r < 0 = 0
            f (i,r)
                | eq ! i = (q-1) * dp ! (i-1,r-2) + dp ! (i-1,r)
                | otherwise = 2 * dp ! (i-1,r-1) + (q-2) * dp ! (i-1,r-2)

    print (dp ! (n,d))
