import           Data.Array            (listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    pat:_:xs <- C.getContents <&> C.lines
    xs
        & filter (match pat)
        & C.unlines
        & C.putStr

match :: C.ByteString -> C.ByteString -> Bool
match pat s = arr ! (0,0)
  where
    m = C.length pat
    n = C.length s

    rng = ((0,0),(m,n))
    arr = listArray rng (map f (range rng))

    f (i,j)
        | i == m && j == n = True
        | i == m = False
        | C.index pat i /= '*' = j /= n && C.index pat i == C.index s j && arr ! (i+1,j+1)
        | j == n = arr ! (i+1,j)
        | otherwise = arr ! (i,j+1) || arr ! (i+1,j+1) || arr ! (i+1,j)
