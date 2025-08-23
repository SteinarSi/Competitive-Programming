import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST)
import           Data.Array.Base       (STUArray, UArray, listArray, newArray,
                                        readArray, writeArray, (!))
import           Data.Array.ST         (runSTUArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> map readInt
            >>> (\[a,b] -> psum ! b - psum ! (a-1))
            >>> show)
        >>> unlines
        >>> putStr
    )

happy :: UArray Int Bool
happy = runSTUArray $ do
        seen <- newArray (1,m) False
        happy <- newArray (1,m) False
        writeArray happy 1 True
        writeArray seen 1 True
        mapM_ (search seen happy []) [2..m]
        pure happy
    where
        search :: STUArray s Int Bool -> STUArray s Int Bool -> [Int] -> Int -> ST s ()
        search seen happy xs x = do
            s <- readArray seen x
            if s
                then readArray happy x >>= flip when (mapM_ (flip (writeArray happy) True) (x:xs))
                else writeArray seen x True >> search seen happy (x:xs) (happify x)

psum :: UArray Int Int
psum = [1..1000000]
        & map (happify >>> (happy!) >>> bool 0 1)
        & scanl (+) 0
        & listArray (0,1000000)

happify :: Int -> Int
happify = digits >>> map (^2) >>> sum
    where
        digits :: Int -> [Int]
        digits 0 = []
        digits x = x `mod` 10 : digits (x `div` 10)

m :: Int
m = 9*9*7

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
