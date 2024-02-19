import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t <- C.getLine <&> readInt
    replicateM_ t $ do
        n <- C.getLine <&> readInt
        replicateM n C.getLine >>= (
                    C.transpose
                >>> map (
                        C.filter ('1'==)
                    >>> C.length
                    >>> (>(n`div`2))
                    >>> bool '0' '1'
                    )
                >>> C.pack
                >>> C.putStrLn
            )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
