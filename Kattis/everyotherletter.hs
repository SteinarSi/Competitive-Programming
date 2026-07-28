import           Control.Arrow ((>>>))

main :: IO ()
main = interact (zip [1..] >>> filter (fst >>> odd) >>> map snd)
