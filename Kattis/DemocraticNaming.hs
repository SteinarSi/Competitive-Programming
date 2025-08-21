import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (minimumBy)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> C.transpose
        >>> map frequentiest
        >>> putStrLn
    )

frequentiest :: C.ByteString -> Char
frequentiest xs = runST $ do
    ret <- newArray ('a','z') 0 :: ST s (STUArray s Char Int)
    C.foldr (\x r -> (readArray ret x >>= (succ >>> writeArray ret x)) >> r) (pure ()) xs
    getAssocs ret <&> (minimumBy (compare `on` (\(x,c) -> (-c,x))) >>> fst)
