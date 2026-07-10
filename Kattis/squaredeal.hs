import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (permutations)

main :: IO ()
main = do
    rectangles <- getContents <&> (lines >>> map (words >>> map read))
    let possible = or $ do
            [r1,r2,r3] <- permutations rectangles
            [w1,h1] <- permutations r1
            [w2,h2] <- permutations r2
            [w3,h3] <- permutations r3
            [h1 == h2 && h1 == h3 && h1 == w1+w2+w3, h1 + h2 == h3 && w1 == w2 && w1 + w3 == h3]
    putStrLn $ if possible
        then "YES"
        else "NO"
