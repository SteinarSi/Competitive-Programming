import           Control.Applicative   (liftA2)
import           Control.Arrow         ((&&&), (***), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray (..), STUArray, getElems,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> solve
        >>> map show
        >>> unwords
        >>> putStrLn
    )

solve :: [[(Int,Int)]] -> [Int]
solve xss = runST $ do
    activity <- newArray (1,365) False
    shares   <- newArray (1,365) 0
    mapM_ (track activity shares 0) xss
    liftA2 zip (getElems activity) (getElems shares) <&> (filter fst >>> map snd >>> scanl1 (+))
  where
    track :: STUArray s Int Bool -> STUArray s Int Int -> Int -> [(Int,Int)] -> ST s ()
    track activity shares prev []     = pure ()
    track activity shares prev ((d,k):xs) = do
        writeArray activity d True
        modifyArray shares d ((k-prev)+)
        track activity shares k xs

parse :: [[Int]] -> [[(Int,Int)]]
parse [] = []
parse ([n]:xss) = splitAt n xss
    & (map (last &&& head) >>> sort) *** parse
    & uncurry (:)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
