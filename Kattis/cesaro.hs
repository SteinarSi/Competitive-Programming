import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn $ case compare (sum xs) 0 of
        LT -> "-INFINITY"
        GT -> "INFINITY"
        EQ -> scanl1 (+) xs
            & take n
            & sum
            & fromIntegral
            & (/ fromIntegral n)
            & show

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
