import           Control.Arrow         (Arrow (first), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, newArray_,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (readInt >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> C.ByteString
solve n = runST $ do
        cards <- newArray_ (0,n-1)
        shuffle cards 1 [0..n-1]
        getElems cards <&> (map show >>> unwords >>> C.pack)
    where
        shuffle :: STUArray s Int Int -> Int -> [Int] -> ST s ()
        shuffle cards i xs | i > n     = pure ()
                           | otherwise = let y:ys = move i xs
                                         in  writeArray cards y i >> shuffle cards (i+1) ys

        move :: Int -> [Int] -> [Int]
        move 0 ys     = ys
        move i (y:ys) = move (i-1) (ys <> [y])

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
