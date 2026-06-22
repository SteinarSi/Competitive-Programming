import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xss <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        dp :: Array Int (Int,Int)
        dp = listArray rng (zipWith f [1..] (xss <> [[]]))
          where
            rng = (1,n)

            f :: Int -> [Int] -> (Int,Int)
            f i xs
                | i == n    = (0,0)
                | otherwise = minimum (zipWith (\j x -> (fst (dp ! j) + x, j)) [i+1..] xs)

        backtrack :: Int -> [Int]
        backtrack i
            | i == n = [i]
            | otherwise = i : backtrack (snd (dp ! i))

    putStrLn (unwords (map show (backtrack 1)))
    print (fst (dp ! 1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
