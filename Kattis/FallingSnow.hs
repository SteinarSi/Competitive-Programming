import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> C.transpose
        >>> map fall
        >>> C.transpose
        >>> C.unlines
        >>> C.putStr
    )

fall :: C.ByteString -> C.ByteString
fall xs = C.replicate (C.length xs - s) '.' <> C.replicate s 'S'
    where
        s = C.count 'S' xs
