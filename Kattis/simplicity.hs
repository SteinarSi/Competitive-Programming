import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)

main :: IO ()
main = C.getContents >>= (
            C.init
        >>> count
        >>> drop 2
        >>> sum
        >>> print
    )

count :: C.ByteString -> [Int]
count xs = runST $ do
    c <- newArray ('a','z') 0 :: ST s (STUArray s Char Int)
    C.foldr' (\x -> ((readArray c x >>= (succ >>> writeArray c x)) >>)) (pure ()) xs
    getElems c <&> sortOn negate
