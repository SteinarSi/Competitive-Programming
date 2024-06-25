import           Control.Arrow         ((>>>))
import           Control.Monad         (ap)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let arr = listArray (1,n) xs :: UArray Int Int

    [1..n`div`2]
        & find (\k -> [k,2*k..n]
            & map (arr!)
            & ap (zipWith (<)) tail
            & and)
        & maybe "ABORT!" show
        & putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
