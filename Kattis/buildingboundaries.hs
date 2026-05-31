import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (permutations)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: [Int] -> Int
solve [x1,y1,x2,y2,x3,y3] = minimum $ do
    xs <- permutations [(x1,y1),(x2,y2),(x3,y3)]
    (w,h) <- combinations xs
    pure (w * h)

combinations :: [(Int,Int)] -> [(Int,Int)]
combinations [] = []
combinations [(x,y)] = [(x,y),(y,x)]
combinations ((x,y):xs) = do
    (w1,h1) <- [(x,y),(y,x)]
    (w2,h2) <- combinations xs
    [(w1+w2,max h1 h2),(max w1 h2,h1+w2)]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
