import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    n <- C.getLine
    xs <- C.getLine

    let
        n = C.length xs
        rng = ((0,0),(n,n))

        dp :: Array (Int,Int) Bool
        dp = listArray rng (map f (range rng))

        f :: (Int,Int) -> Bool
        f (i,o) | i >= n = o == 0
                | otherwise = case C.index xs i of
                    '(' -> dp ! (i+1,o+1)
                    ')' -> o > 0 && dp ! (i+1,o-1)
                    '*' -> o > 0 && dp ! (i+1,o-1) || dp ! (i+1,o+1)

    putStrLn $ if dp ! (0,0)
        then "YES"
        else "NO"
