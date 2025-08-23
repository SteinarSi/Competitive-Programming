import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)

    let ans | k >= 10^9 = withSet S.empty k  xs
            | otherwise = runST (newArray (0,10^9) False >>= withArray k xs)

    putStrLn $ case ans of
            Nothing    -> "Neibb"
            Just (a,b) -> show a <> " " <> show b

withSet :: S.IntSet -> Int -> [Int] -> Maybe (Int,Int)
withSet seen k [] = Nothing
withSet seen k (x:xs) | x > k               = withSet seen k xs
                      | S.member (k-x) seen = Just (x,k-x)
                      | otherwise           = withSet (S.insert x seen) k xs

withArray :: Int -> [Int] -> STUArray s Int Bool -> ST s (Maybe (Int,Int))
withArray k [] seen = pure Nothing
withArray k (x:xs) seen
        | x > k = withArray k xs seen
        | otherwise = do
            y <- readArray seen (k-x)
            if y
                then pure (Just (x, k-x))
                else writeArray seen x True >> withArray k xs seen

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
