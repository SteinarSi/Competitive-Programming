import           Control.Arrow         ((>>>))
import           Data.Bits             (popCount, (.&.), (.|.))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> \[x,y] -> show (solve x y))
        >>> unlines
        >>> putStr
    )

solve :: Int -> Int -> Int
solve x y | x .&. y /= 0 || 2^p-1 /= x .|. y = -1
          | otherwise = p
  where
    p = popCount x + popCount y

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
