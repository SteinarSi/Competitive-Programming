import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    intervals <- fmap (map ((\(a:b:_) -> (a,b)) . map (fst . fromJust . C.readInt) . C.words) . tail . C.lines) C.getContents
    let (x:xs) = sortOn snd intervals
    print (1 + schedule x xs)

schedule :: (Int,Int) -> [(Int,Int)] -> Int
schedule _ [] = 0
schedule (a,b) ((x,y):xs) | x >= b = 1 + schedule (x,y) xs
                          | otherwise = schedule (a,b) xs
