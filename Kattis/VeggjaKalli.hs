import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n',m',xs] <- C.getContents <&> C.words

    let n = readInt n'
        m = readInt m'
        psum = C.foldr ((=='#') >>> bool 0 1 >>> (:)) [] xs
            & scanl1 (+)
            & listArray (0,n-1) :: UArray Int Int
        options = [1..n-m-1]
            & filter (\i -> C.index xs (i-1) == '#' && C.index xs (i+m) == '#')
            & map (\i -> psum ! (i+m-1) - psum ! (i-1))

    putStrLn $ if null options
        then "Neibb"
        else show (minimum options)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
