import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,_,b] <- getLine <&> (words >>> map read)

    putStrLn $ case compare (a::Int) b of
        LT -> "<"
        EQ -> "Goggi svangur!"
        GT -> ">"
