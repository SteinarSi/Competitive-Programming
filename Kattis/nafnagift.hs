import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    xs:ys:_ <- C.getContents <&> C.lines
    C.putStrLn $ solve xs ys

solve :: C.ByteString -> C.ByteString -> C.ByteString
solve xs ys = backtrack (n,m) 
        & reverse
        & C.pack
    where
        n = C.length xs
        m = C.length ys

        dp :: Array (Int,Int) Int
        dp = listArray ((0,0),(n,m)) $ map f (range ((0,0),(n,m)))
        
        f (0,j) = j
        f (i,0) = i
        f (i,j) | xs `C.index` (i-1) == ys `C.index` (j-1) = dp ! (i-1,j-1)
                | otherwise = 1 + min (dp ! (i-1,j)) (dp ! (i,j-1))

        backtrack :: (Int,Int) -> String
        backtrack (0,0) = ""
        backtrack (i,0) = xs `C.index` (i-1) : backtrack (i-1,0)
        backtrack (0,j) = ys `C.index` (j-1) : backtrack (0,j-1)
        backtrack (i,j) | xs `C.index` (i-1) == ys `C.index` (j-1) = xs `C.index` (i-1) : backtrack (i-1,j-1)
                        | dp ! (i-1,j) < dp ! (i,j-1)              = xs `C.index` (i-1) : backtrack (i-1,j)
                        | otherwise                                = ys `C.index` (j-1) : backtrack (i,j-1)
