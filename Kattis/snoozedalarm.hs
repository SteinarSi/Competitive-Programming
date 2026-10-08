import           Control.Arrow         ((>>>))
import           Data.Array.ST         (STUArray, modifyArray, newArray,
                                        runSTUArray)
import           Data.Array.Unboxed    (UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    let
        count :: UArray Int Int
        count = runSTUArray $ do
            ret <- newArray (0,1439) 0
            mapM_ (flip (modifyArray ret) succ) xs
            pure ret

        attempt :: Int -> Int
        attempt k = n - sum (map (\i -> min (count ! i) (count ! (i+k))) [0..1439-k])

    putStrLn (show (minimum (map attempt [1..1439])) <> " " <> show n)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
