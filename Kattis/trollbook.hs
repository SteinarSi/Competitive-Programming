import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STArray, getElems, newArray_,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map C.words)

    C.putStrLn $ runST $ do
        book <- newArray_ (1,readInt n) :: ST s (STArray s Int C.ByteString)
        mapM_ (\[x,p] -> writeArray book (readInt p) x) xs
        getElems book <&> C.unwords

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
