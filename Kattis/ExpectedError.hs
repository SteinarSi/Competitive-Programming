import           Control.Arrow ((>>>))
import           Data.Function (on, (&))
import           Data.Functor  ((<&>))
import           Data.List     (minimumBy)

main :: IO ()
main = do
    [n,k,p'] <- getLine <&> (words >>> map read)

    let p = p' / 100
        typing = 0.1
        backspace = 0.1
        submit = 0.1
        restart = 0.3
        realize = 0.3
        whole = typing * n + submit
        rest = typing * (n-k) + submit

    [
        ("restart", restart + whole),
        ("backspace", p * (backspace + rest) + (1-p) * (backspace + rest + realize + whole)),
        ("continue", p * (rest + realize + whole) + (1-p) * rest)
        ]
        & minimumBy (compare `on` snd)
        & fst
        & putStrLn
