import           Data.Bool (bool)

main :: IO ()
main = do
    wc:hc:ws:hs:_ <- fmap (map read . words) getLine
    print . bool 0 1 $ wc > ws + 1 && hc > hs + 1
