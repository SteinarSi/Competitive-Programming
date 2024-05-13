import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, isDigit, ord)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)


main :: IO ()
main = do
    n <- C.getLine <&> readInt
    ls <- C.getContents <&> C.lines

    let matrix = take n ls & map (C.words >>> map readInt)
        cipher = concatMap (transform matrix) (
                    last ls
                & C.unpack
                & pad n
                & map sub
                & chunksOf n
            )
            & map unSub

    putStrLn cipher

pad :: Int -> String -> String
pad n xs | length xs `mod` n == 0 = xs
         | otherwise = xs ++ replicate (n - length xs `mod` n) ' '

sub :: Char -> Int
sub ' ' = 36
sub  c | isDigit c = ord c - ord '0' + 26
       | otherwise = ord c - ord 'A'

unSub :: Int -> Char
unSub 36 = ' '
unSub c | c <= 25 = chr (ord 'A' + c)
        | otherwise = chr (ord '0' + c - 26)

transform :: [[Int]] -> [Int] -> [Int]
transform matrix vector = map (zipWith (*) vector >>> sum >>> (`mod` 37)) matrix

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)
