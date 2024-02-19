import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    word <- C.getLine
    C.getContents >>= (
                C.lines
            >>> tail
            >>> filter (C.length >>> (>=4))
            >>> filter (C.elem (C.head word))
            >>> filter (C.all (`C.elem` word))
            >>> mapM_ C.putStrLn
        )
