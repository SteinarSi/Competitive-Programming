import           Control.Arrow (first, second)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    xs <- getContents

    putStrLn $ play n (0,0) (0,0) xs

play :: Int -> (Int,Int) -> (Int,Int) -> [Char] -> String
play n total (3,_) xs         = play n (first  succ total) (0,0) xs
play n total (_,3) xs         = play n (second succ total) (0,0) xs
play n (h,a) _     _ | h == n = "Arnar"
                     | a == n = "Hannes"
play n total _    []          = error "bruh"
play n total curr ('H':xs)    = play n total (first  succ curr) xs
play n total curr ('A':xs)    = play n total (second succ curr) xs
play n total curr (_:xs)      = error "bruh"
