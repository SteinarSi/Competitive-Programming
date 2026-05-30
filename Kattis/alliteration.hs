import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha, ord)
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (intersperse, maximumBy)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map C.head >>> favorite)
        >>> intersperse '\n'
        >>> putStrLn
    )

favorite :: [Char] -> Char
favorite xs = runST $ do
    count <- newArray ('a','z') 0 :: ST s (STUArray s Char Int)
    mapM_ (\x -> readArray count x >>= (succ >>> writeArray count x)) (filter isAlpha xs)
    getAssocs count <&> (maximumBy (compare `on` (\(a,c) -> (c, - ord a))) >>> fst)
