import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (sortOn)
import qualified Data.Map      as M

main :: IO ()
main = do
    xs <- getContents <&> (lines >>> drop 1 >>> map (words >>> score) >>> M.fromListWith max >>> M.toList >>> sortOn (snd >>> negate))
    putStrLn $ case xs of
        (n1,s1):(n2,s2):_ | s1 == s2 -> "tie " <> show s1
        (n,s):_                      -> n <> " " <> show s

score :: [String] -> (String,Int)
score [name,h,w,[h1,h2,_,m1,m2]] = (name,score)
  where
    clock = (60 * read [h1,h2] + read [m1,m2]) `mod` (12*60)
    clockScore
        | clock <= 3*60 = fromInteger (3*60-clock) / (3*60)
        | clock >= 9*60 = fromInteger (clock-9*60) / (3*60)
        | otherwise     = 0
    score = floor ((5*read h + read w) * clockScore)
