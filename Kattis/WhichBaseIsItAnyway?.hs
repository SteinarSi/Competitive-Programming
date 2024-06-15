import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         ((&))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> (\(a:b:_) -> solve a b)
            >>> C.putStrLn
        )
    )

solve :: C.ByteString -> C.ByteString -> C.ByteString
solve i x = C.unwords [i, oct, convert 10, convert 16]
    where
        oct | C.elem '8' x || C.elem '9' x = C.pack "0"
            | otherwise = convert 8
        convert b = C.unpack x
            & map digitToInt
            & reverse
            & zipWith (*) (map (b^) [0..])
            & sum
            & show
            & C.pack
