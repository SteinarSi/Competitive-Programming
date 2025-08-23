import           Control.Arrow         ((***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    time <- C.getContents <&> (C.lines >>> drop 1 >>> map parse >>> timeline)

    let winner | fst (last time) > 0 = "H"
               | otherwise             = "A"
        (h,a) = count (0,0) time

    printf "%s %s %s\n" winner (showTime h) (showTime a)

count :: (Int,Int) -> [(Int,Int)] -> (Int,Int)
count (h,a) [] = (h,a)
count (h,a) [(p,t)] = case compare p 0 of
    GT -> (end-t+h,a)
    LT -> (h,end-t+a)
    EQ -> (h,a)
count (h,a) ((p1,t1):(p2,t2):xs) = count s ((p2,t2):xs)
  where
    s = case compare p1 0 of
        GT -> (h+t2-t1, a)
        LT -> (h, a+t2-t1)
        EQ -> (h,a)

timeline :: [(Int,Int)] -> [(Int,Int)]
timeline = scanl (\(s,_) (p,t) -> (p+s,t)) (0,0)

parse :: C.ByteString -> (Int,Int)
parse xs = let [ha,p,t] = C.words xs
           in  (bool (-1) 1 (C.head ha == 'H') * readInt p, readTime t)

readTime :: C.ByteString -> Int
readTime = C.readInt >>> fromJust >>> (60*) *** (C.tail >>> readInt) >>> uncurry (+)

showTime :: Int -> String
showTime x = let (m,s) = quotRem x 60
             in  printf "%d:%.2d" m s

end :: Int
end = 32*60

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
