import           Control.Category      ((>>>))
import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    m <- C.getLine <&> readInt
    when (m /= 0) $ do
        rects <- getRectangles
        let (w,h) = solve m rects
        putStrLn (show w ++ " x " ++ show h)
        main

solve :: Int -> [(Int,Int)] -> (Int,Int)
solve _ [] = (0,0)
solve m xs = let ((w1,h1),ys) = solve' 0 0 xs
                 (w2,h2) = solve m ys
             in  (max w1 w2, h1+h2)
    where
        solve' :: Int -> Int -> [(Int,Int)] -> ((Int,Int),[(Int,Int)])
        solve' w h [] = ((w,h),[])
        solve' w h ((x,y):xs) | w+x > m = ((w,h),(x,y):xs)
                              | otherwise = solve' (w+x) (max h y) xs

getRectangles :: IO [(Int,Int)]
getRectangles = do
    [w,h] <- C.getLine <&> (C.words >>> map readInt)
    if (w,h) /= (-1,-1)
        then ((w,h) :) <$> getRectangles
        else pure []

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
