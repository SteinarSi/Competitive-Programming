import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    initial <- guess (S.fromList ['a'..'z'])
    let start = replicate (length initial) (S.fromList ['a'..'z'])
    play guesses (zip [1..] start)

play :: [S.Set Char] -> [(Int,S.Set Char)] -> IO ()
play [] xs = putStrLn ('!' : ' ' : map (snd >>> S.findMin) xs)
play (g:gs) xs = guess g >>= (adjust xs >>> play gs)
  where
    adjust :: [(Int,S.Set Char)] -> [Int] -> [(Int,S.Set Char)]
    adjust [] _ = []
    adjust xs [] = map (\(x,ks) -> (x, S.difference ks g)) xs
    adjust ((x,ks):xs) (i:is)
        | i == x    = (x, S.intersection g ks) : adjust xs is
        | otherwise = (x, S.difference ks g) : adjust xs (i:is)

guess :: S.Set Char -> IO [Int]
guess xs = do
    putStrLn ('?' : ' ' : S.toList xs)
    hFlush stdout
    C.getLine <&> (C.words >>> drop 1 >>> map readInt)

guesses :: [S.Set Char]
guesses = map S.fromList ["acegikmoqsuwy", "adehilmpqtuxy", "bcdejklmrstuz", "fghijklmvwxyz", "nopqrstuvwxyz"]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
