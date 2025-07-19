import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust, isJust)

main :: IO ()
main = do
    [[n],xs,ys] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    solve n (listArray (1,n) xs) (listArray (1,n) ys)
        & format ys
        & unwords
        & putStrLn

format :: [Int] -> [Int] -> [String]
format xs [i] | length xs == i = map show xs
format xs (i:is) = let (ys,zs) = splitAt i xs
                   in  map show ys <> ["#"] <> format zs is

solve :: Int -> UArray Int Int -> UArray Int Int -> [Int]
solve n xs ys = fromJust (dp ! 1)
  where
    dp :: Array Int (Maybe [Int])
    dp = listArray (1,n+1) (map (g 0 M.empty M.empty) [1..n] <> [Just []])

    g :: Int -> M.IntMap Int -> M.IntMap Int -> Int -> Maybe [Int]
    g c xx yy j
        | c > 0 && xx == yy && isJust (dp ! j) = (c:) <$> dp ! j
        | j > n = Nothing
        | otherwise = g (c+1) (M.insertWith (+) (xs!j) 1 xx) (M.insertWith (+) (ys!j) 1 yy) (j+1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
