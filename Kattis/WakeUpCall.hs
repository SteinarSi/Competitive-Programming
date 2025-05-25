import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [x,y] <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> map readInt >>> sum))
    putStrLn $ case compare x y of
        GT -> "Button 1"
        LT -> "Button 2"
        EQ -> "Oh no"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
