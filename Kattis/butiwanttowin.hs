import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (delete, sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.words >>> map readInt)

    let o = maximum xs
        m = maximum (delete o xs)
        rest = sort (delete m (delete o xs))

    simulate o m (sum rest) rest
        & maybe "IMPOSSIBLE TO WIN" show
        & putStrLn

simulate :: Int -> Int -> Int -> [Int] -> Maybe Int
simulate o m t _ | o + t < m = Just 0
simulate o m t [] = Nothing
simulate o m t (x:xs) = simulate o (m+a) (t-a) zs <&> succ
  where
    (ys,zs) = span (x==) xs
    a = x * (1 + length ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
