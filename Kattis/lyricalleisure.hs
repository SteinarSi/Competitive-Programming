import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (delete)

main :: IO ()
main = do
    [n,k] <- getLine <&> (words >>> map read)
    putStrLn (replicate k 'Z' <> take (n-k) generation)

generation :: String
generation = concatMap (`gen` ['A'..'Z']) [1..]
  where
    gen :: Int -> String -> String
    gen 0 xs = xs
    gen i xs = concat [gen (i-1) (delete x xs) | x <- xs]
