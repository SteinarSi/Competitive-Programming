import           Control.Arrow         ((>>>))
import           Data.Array.Base       (assocs)
import           Data.Array.ST         (newArray, readArray, runSTUArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:h:xs <- C.getContents <&> (C.words >>> map readInt)

    let distribution = (runSTUArray $ do
                ret <- newArray (1,10^6) (0::Int)
                mapM_ (\x -> readArray ret x >>= (succ >>> writeArray ret x)) xs
                pure ret)
            & assocs
            & filter (snd >>> (>0))
            & sortOn (snd >>> negate)
            & distribute h [] (0,[]) []

    putStr $ case distribution of
        [] -> "impossible\n"
        ys -> unlines (map (map show >>> unwords) ys)

distribute :: Int -> [[Int]] -> (Int,[Int]) -> [(Int,Int)] -> [(Int,Int)] -> [[Int]]
distribute h ret (c,cs) used xs | c == h = distribute h (cs:ret) (0,[]) [] (merge (reverse used) xs)
distribute h ret (c,cs) used [] = ret
distribute h ret (c,cs) used ((x,xc):xs)
    | xc <= 1   = distribute h ret (c+1,x:cs) used xs
    | otherwise = distribute h ret (c+1,x:cs) ((x,xc-1):used) xs

merge :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
merge [] ys = ys
merge xs [] = xs
merge ((x,xc):xs) ((y,yc):ys)
    | xc >= yc  = (x,xc) : merge xs ((y,yc):ys)
    | otherwise = (y,yc) : merge ((x,xc):xs) ys

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
