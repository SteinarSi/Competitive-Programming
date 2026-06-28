import           Control.Arrow ((***), (>>>))
import           Data.Array    (range)
import           Data.Function (on, (&))
import           Data.Functor  ((<&>))
import           Data.List     (minimumBy, partition)
import           System.IO     (hFlush, stdout)

main :: IO ()
main = play (range ((1,1),(4,4)))

play :: [(Int,Int)] -> IO ()
play [] = error "wtf??"
play [(y,x)] = putStrLn ("! " <> show y <> " " <> show x)
play xs = do
    let g@(y,x) = minimumBy (compare `on` hit >>> flip partition xs >>> length *** length >>> uncurry (-) >>> abs) (range ((1,1),(5,5)))
    putStrLn ("? " <> show y <> " " <> show x)
    hFlush stdout
    a <- getLine <&> (=="1")
    if a
        then play (filter (hit g) xs)
        else play (filter (hit g >>> not) xs)

hit :: (Int,Int) -> (Int,Int) -> Bool
hit (y,x) (ty,tx) = (y == ty || y == ty+1) && (x == tx || x == tx+1)
