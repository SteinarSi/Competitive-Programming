import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (Empty, (:<|), (:|>)), fromList,
                                        singleton)

main :: IO ()
main = do
    m <- fmap (C.words >>> last >>> readInt) C.getLine
    xs <- fmap (C.words >>> map (readInt >>> even >>> bool 0 1)) C.getContents
    let (start, rest) = splitAt m xs
    print $ solve (bool 0 1 (sum start >= 2)) (sum start) (fromList start) rest

solve :: Int -> Int -> Seq Int -> [Int] -> Int
solve ret _ _ [] = ret
solve ret curr (x :<| xs) (y:ys) = solve ret' next (xs <> singleton y) ys
    where
        next = curr + y - x
        ret' | next >= 2 = ret + 1
             | otherwise = ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
