import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, inRange, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

unique :: Int
unique = 9182435251251534513

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map solve
        >>> unlines
        >>> putStr
    )

solve :: (Int, UArray Int Int, UArray Int Int) -> String
solve (n, ps, as) = runST (do
        opt  <- newArray ((False,1),(True,n)) unique
        seen <- newArray ((False,1),(True,n)) False
        mapM (play opt seen True 0) [1..n])
    & map show
    & unwords
  where
    play :: STUArray s (Bool,Int) Int -> STUArray s (Bool,Int) Bool -> Bool -> Int -> Int -> ST s Int
    play opt seen maximize score u = do
        let a = as ! u
        prev <- readArray opt (maximize,u)
        if prev == unique
            then do
                writeArray seen (maximize,u) True
                writeArray opt  (maximize,u) score

                ret <- [u + ps!u, u + 2*ps!u..]
                    & takeWhile (inRange (1,n))
                    & mapM (play opt seen (not maximize) (score+a))
                    <&> (map (a+) >>> (a:) >>> bool minimum maximum maximize)

                writeArray seen (maximize,u) False
                writeArray opt (maximize,u) ret
                pure ret
            else do
                s <- readArray seen (maximize,u)
                if s
                    then do
                        writeArray seen (maximize,u) False
                        pure $ case compare score prev of
                            LT -> minBound
                            EQ -> 0
                            GT -> maxBound
                    else do pure prev


parse :: [[Int]] -> [(Int, UArray Int Int, UArray Int Int)]
parse []             = []
parse ([n]:ps:as:xs) = (n, listArray (1,n) ps, listArray (1,n) as) : parse xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
