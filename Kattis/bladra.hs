import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    k <- C.getLine <&> readInt
    solved <- C.getContents <&> (
                C.lines
            >>> map (C.words
                >>> last
                >>> readInt
            )
        )

    print $ runST $ do
        count <- newArray (1,k) 0 :: ST s (STUArray s Int Int)
        forM_ solved (\p -> readArray count p >>= (succ >>> writeArray count p))
        getElems count <&> minimum

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
