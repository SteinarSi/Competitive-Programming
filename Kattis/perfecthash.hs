import           Control.Arrow ((>>>))
import           Data.Char     (ord)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (find)
import qualified Data.Set      as S

main :: IO ()
main = do
    xss <- getContents <&> (lines >>> drop 1)
    let n = length xss
        Just (m,a) = find (\(m,a) -> S.size (S.fromList (map (hash a m 0) xss)) == n) [(m,a) | m <- [n..], a <- [1..m-1]]
    putStrLn $ if n <= 1
        then "0 1"
        else show a <> " " <> show m

hash :: Int -> Int -> Int -> String -> Int
hash a m h []     = h
hash a m h (x:xs) = hash a m ((a * h + ord x) `mod` m) xs
