import           Control.Arrow         (second, (&&&), (>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getElems, newArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,m):xs <- C.getContents <&> (C.lines
        >>> map (C.words
            >>> map readInt
            >>> (head &&& (!!1))))
    let grid = runST $ do
            grid' <- newArray ((1,1),(n,m)) '.' :: ST s (STUArray s (Int,Int) Char)
            forM_ xs (flip (writeArray grid') '*')
            getElems grid'
    chunksOf m grid
        & map C.pack
        & C.unlines
        & C.putStr

chunksOf :: Int -> [a] -> [[a]]
chunksOf k xs | null xs = []
              | otherwise = splitAt k xs
                    & second (chunksOf k)
                    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
