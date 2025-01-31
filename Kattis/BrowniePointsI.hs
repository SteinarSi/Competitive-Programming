import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> split
        >>> map (\(xy,xs) -> play xy (0,0) xs)
        >>> C.unlines
        >>> C.putStr
    )

play :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> C.ByteString
play (x,y) (s,o) [] = C.pack (show s <> " " <> show o)
play (x,y) (s,o) ((x',y'):xs) = play (x,y) score xs
    where
        score = case (compare x' x, compare y' y) of
            (LT,LT) -> (s+1,o)
            (GT,GT) -> (s+1,o)
            (LT,GT) -> (s,o+1)
            (GT,LT) -> (s,o+1)
            _       -> (s,o)

split :: [(Int,Int)] -> [((Int,Int),[(Int,Int)])]
split []      = []
split [(0,_)] = []
split ((n,_):xs) = let (a,b) = splitAt n xs
                   in  (a !! (n`div`2), a) : split b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
