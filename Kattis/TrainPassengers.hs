import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [c,n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    putStrLn $ if possible c 0 xs
        then "possible"
        else "impossible"

possible :: Int -> Int -> [[Int]] -> Bool
possible cap curr [] = True
possible cap curr [[left,entered,stayed]] = left == curr && entered == 0 && stayed == 0
possible cap curr ([left,entered,stayed]:xs)
        | left > curr = False
        | next > cap = False
        | next < 0 = False
        | stayed > 0 && cap /= next = False
        | otherwise = possible cap next xs
    where
        next = curr - left + entered

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
