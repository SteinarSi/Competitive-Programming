import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n',p',d',xs] <- C.getContents <&> C.words

    let [n,p,d] = map readInt [n',p',d']
        prefix = C.unpack xs
            & map ((=='Z') >>> bool 0 1)
            & cycle
            & scanl (+) 0
            & listArray (0,n+p) :: UArray Int Int

    print $ length [()| i <- [1..n], prefix ! (i+p-1) - prefix ! (i-1) < d]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
