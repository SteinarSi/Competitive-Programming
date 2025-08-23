{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           GHC.Arr               (STArray)

main :: IO ()
main = do
    [p,d] <- fmap (C.words >>> map readInt) C.getLine
    precincts <- C.getContents <&> (C.lines
            >>> map (C.words >>> map readInt >>> (\[a,b,c] -> (a,(b,c))))
        )
    let results = runST $ do
            arr <- newArray (1,d) (0,0) :: ST s (STArray s Int (Int,Int))
            forM_ precincts $ \(i,(a,b)) -> do
                (a',b') <- readArray arr i
                writeArray arr i (a+a',b+b')
            getElems arr
    mapM_ C.putStrLn (solve 0 (0,0) results)

solve :: Int -> (Int,Int) -> [(Int,Int)] -> [C.ByteString]
solve v (a,b) [] = [C.pack (show (fromIntegral (abs (a-b)) / fromIntegral v))]
solve v (a,b) ((x,y):xs) = C.unwords (waste : map (show>>>C.pack) [wa,wb])  : solve (v+x+y) (a+wa,b+wb) xs
    where
        (waste, wa, wb) | x > y     = ("A ", x-((x+y)`div`2+1), y)
                        | otherwise = ("B ", x, y-((x+y)`div`2+1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
