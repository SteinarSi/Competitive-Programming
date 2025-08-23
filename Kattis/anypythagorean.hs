import Control.Arrow ((>>>))
import Data.Functor  ((<&>))
import Control.Monad (guard)

main :: IO ()
main = do
    n <- getContents <&> read

    let triples = do
            c <- [(n+2) `div` 3 .. n `div` 2]
            a <- [1 .. (n-c) `div` 2]
            let b = n - c - a
            guard (a^2 + b^2 == c^2)
            pure (a,b,c)

    putStrLn $ case triples of
        [] -> "0 0 0"
        ((a,b,c):_) -> unwords (map show [a,b,c])
