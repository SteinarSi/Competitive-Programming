import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (listArray, (!))
import           Data.Array.ST         (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([n,k],xs) <- C.getContents <&> (C.lines >>> (head >>> C.words >>> map readInt) &&& (drop 1 >>> map (C.words >>> (head >>> C.unpack) &&& (last >>> readInt)) >>> sortOn (snd >>> negate)))
    let id2color = listArray (1,k) (map fst xs)
        cs = zipWith (\i (_,c) -> (i,c)) [1..] xs
    putStrLn $ case solve n cs of
        Nothing -> "Nej"
        Just xs -> "Ja\n" <> unwords (map (id2color!) xs)

solve :: Int -> [(Int,Int)] -> Maybe [Int]
solve n xs = runST $ do
    color <- newArray (1,n) (-1)
    ret <- distribute color xs ([1,3..n] <> [2,4..n])
    if ret
        then Just <$> getElems color
        else pure Nothing
  where
    distribute :: forall s. STUArray s Int Int -> [(Int,Int)] -> [Int] -> ST s Bool
    distribute _ [] _ = pure True
    distribute color ((_,0):xs) is = distribute color xs is
    distribute color xs [] = error ("bruh???" <> show xs)
    distribute color ((x,c):xs) (i:is) = do
        l <- readLoop (i-1)
        r <- readLoop (i+1)
        if x == l || x == r
            then pure False
            else writeArray color i x >> distribute color ((x,c-1):xs) is
      where
        readLoop :: Int -> ST s Int
        readLoop 0             = readArray color n
        readLoop i | i == n+1  = readArray color 1
                   | otherwise = readArray color i

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
