import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> (head >>> C.head) &&& (last >>> readInt))
        >>> solulu M.empty 0 0
        >>> concat
        >>> putStr
    )

solulu :: M.IntMap Int -> Int -> Int -> [(Char,Int)] -> [String]
solulu ages tot n []          = []
solulu ages tot n ((ar,x):xs) = result : solulu ages' tot' n' xs
  where
    m = M.findWithDefault 0 x ages
    (ages',tot',n') = case ar of
        'A' -> (M.insert x (m+1) ages, tot+x, n+1)
        'R' | m <= 1    -> (M.delete x ages, tot-x, n-1)
            | otherwise -> (M.insert x (m-1) ages, tot-x, n-1)
    result | n' <= 0   = "-1 -1 -1\n"
           | otherwise = printf "%d %d %.4f\n" (fst (M.findMin ages')) (fst (M.findMax ages')) (fromIntegral tot' / fromIntegral n' :: Double)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
