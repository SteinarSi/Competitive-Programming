import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (find)

main :: IO ()
main = do
    xs <- C.getLine
    ys <- C.getLine

    let n = C.length xs
        Just l = find different [0..n-1]
        Just r = find different [n-1,n-2..0]
        different i = xs `C.index` i /= ys `C.index` i

    print $ if substr xs l r /= C.reverse (substr ys l r)
        then 0
        else zip [l,l-1..0] [r,r+1..n-1]
            & takeWhile (\(i,j) -> xs `C.index` i == ys `C.index` j)
            & length

substr :: C.ByteString -> Int -> Int -> C.ByteString
substr xs i j = C.take (j-i+1) (C.drop i xs)
