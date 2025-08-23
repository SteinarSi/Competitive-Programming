import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getLine >>= (C.unpack >>> count 0 0 0 >>> C.putStrLn)

count :: Int -> Int -> Int -> String -> C.ByteString
count a b c "" = C.intersperse '+' (C.replicate a '1' <> C.replicate b '2' <> C.replicate c '3')
count a b c (x:xs) = case x of
    '1' -> count (a+1) b c xs
    '2' -> count a (b+1) c xs
    '3' -> count a b (c+1) xs
    _   -> count a b c xs
