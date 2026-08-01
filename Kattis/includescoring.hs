import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (sort, sortBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    zip [1..] xs
        & sortBy (cmp `on` snd)
        & distribute ([100,75,60,50,45,40,36,32,29,26,24,22,20,18] <> [16,15..1] <> repeat 0)
        & sort
        & map (snd >>> show)
        & unlines
        & putStr

distribute :: [Int] -> [(Int,[Int])] -> [(Int,Int)]
distribute _ [] = []
distribute ps (x:xs) = map (second (last >>> (+p))) ys <> distribute rs zs
  where
    (ys,zs) = span (snd >>> cmp (snd x) >>> (==EQ)) (x:xs)
    (qs,rs) = splitAt l ps
    l = length ys
    p = (sum qs + l - 1) `div` l

cmp :: [Int] -> [Int] -> Ordering
cmp [s1,p1,f1,_] [s2,p2,f2,_] = compare (-s1) (-s2) <> compare p1 p2 <> compare f1 f2

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
