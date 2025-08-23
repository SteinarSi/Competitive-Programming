import           Control.Arrow            ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe               (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> (\(a:o:b:_) -> (C.head o, readInt a, readInt b)))
        >>> calculate 1
        >>> map (show >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

calculate :: Int -> [(Char, Int, Int)] -> [Int]
calculate _ [] = []
calculate prev ((op, a, b):xs) = val : calculate val xs
    where
        val = case op of
            '+' -> (a+b) - prev
            '-' -> (a-b) * prev
            '*' -> (a*b)^2
            '/' -> (a+1) `div` 2
            _ -> undefined

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
