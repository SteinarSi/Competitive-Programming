import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [h1,w1,h2,w2] <- getLine <&> (words >>> map read)
    putStr $ unlines $ replicate h2 (replicate w2 '-' <> replicate (w1-w2) '+')
    putStr $ unlines $ replicate (h1-h2) (replicate w1 '+')
