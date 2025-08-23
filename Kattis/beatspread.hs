import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> map readInt
            >>> (\(a:b:_) -> solve a b a 0)
            >>> C.putStrLn
        )
    )

solve :: Int -> Int -> Int -> Int -> C.ByteString
solve a b i j | i - j == b = C.unwords $ map (show >>> C.pack) [i, j]
              | j > i = C.pack "impossible"
              | otherwise = solve a b (pred i) (succ j)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
