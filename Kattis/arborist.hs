import           Control.Arrow ((***), (>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (partition, sortOn)

main :: IO ()
main = do
    [[n,k],ps,ws] <- getContents <&> (lines >>> map (words >>> map read))

    print $ if k < 40
        then single ps
        else let (light,heavy) = zip ps ws
                    & partition (snd >>> (==20))
                    & (map fst >>> sortOn negate) *** map fst
             in  single heavy + double light

single :: [Int] -> Int
single = map (\x -> 2*x+2) >>> sum

double :: [Int] -> Int
double []       = 0
double [x]      = 2 + 2*x
double (x:_:xs) = 4 + 2*x + double xs
