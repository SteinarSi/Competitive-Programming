import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [_,x,y] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> sum))

    putStrLn $ case compare x y of
        LT -> "left"
        EQ -> "either"
        GT -> "right"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
