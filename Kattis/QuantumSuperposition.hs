import           Control.Arrow         ((***), (>>>))
import           Data.Array            (Array, listArray, (!))
import           Data.Array.ST         (newArray, readArray, runSTArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    l:ls <- C.getContents <&> C.lines
    let [n1,n2,m1,m2] = map readInt (C.words l)

    splitAt (m1+m2) ls
        & (map (C.words >>> map readInt)
            >>> splitAt m1
            >>> (paths n1 *** paths n2)
            >>> possible)
                ***
            (drop 1 >>> map readInt)
        & uncurry map
        & map (bool "No" "Yes")
        & unlines
        & putStr

possible :: (S.IntSet,S.IntSet) -> Int -> Bool
possible (p1,p2) q = any ((q-) >>> (`S.member` p2)) (S.toList p1)

paths :: Int -> [[Int]] -> S.IntSet
paths n xs = S.fromList (dp ! n)
  where
    dp :: Array Int [Int]
    dp = listArray (1,n) ([0] : map f [2..n])

    f :: Int -> [Int]
    f i = graph ! i
        & concatMap (dp!)
        & S.fromList
        & S.toList
        & map succ

    graph :: Array Int [Int]
    graph = runSTArray $ do
        g <- newArray (1,n) []
        mapM_ (\[v,u] -> readArray g u >>= ((v:) >>> writeArray g u)) xs
        pure g

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
