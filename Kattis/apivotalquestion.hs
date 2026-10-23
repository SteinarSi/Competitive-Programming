import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let maxLeft = scanl max minBound xs
        minRight = drop 1 (reverse (scanl min maxBound (reverse xs)))

    let ans = zip3 maxLeft xs minRight
            & filter (\(mi,x,ma) -> mi < x && x < ma)
            & map (\(_,x,_) -> x)

    putStrLn (unwords (map show (length ans : take 100 ans)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
