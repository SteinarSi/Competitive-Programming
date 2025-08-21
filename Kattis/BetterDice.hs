import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [[n],xs,ys] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    putStrLn $ case compare (sum [signum (x-y) | x <- xs, y <- ys]) 0 of
        LT -> "second"
        EQ -> "tie"
        GT -> "first"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
