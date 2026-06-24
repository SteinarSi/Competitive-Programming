import           Data.Bool    (bool)
import           Data.Functor ((<&>))
import           Data.Tuple   (swap)
import           System.IO    (hFlush, stdout)
import           Text.Printf  (printf)

main :: IO ()
main = single (1,500) >>= uncurry (printf "GUESS %d %d\n") >> hFlush stdout

single :: (Int,Int) -> IO (Int,Int)
single (lo,hi)
    | lo >= hi  = pure (lo,lo)
    | otherwise = do
        let mi = (lo+hi) `div` 2
        g <- guess lo mi
        case g of
            (True,True)   -> single (lo,mi)
            (False,False) -> single (mi+1,hi)
            (True,False)  -> double False (lo,mi) (mi+1,hi)
            (False,True)  -> double True (lo,mi) (mi+1,hi)

double :: Bool -> (Int,Int) -> (Int,Int) -> IO (Int,Int)
double s (lo1,hi1) (lo2,hi2)
    | lo1 >= hi1 && lo2 >= hi2 = pure (lo1,lo2) <&> bool id swap s
    | otherwise = do
        let mi1 = (lo1+hi1+1) `div` 2
            mi2 = (lo2+hi2) `div` 2

        (x,y) <- guess mi1 mi2 <&> bool id swap s

        let lh1 = bool (lo1,mi1-1) (mi1,hi1) x
            lh2 = bool (mi2+1,hi2) (lo2,mi2) y

        double s lh1 lh2

guess :: Int -> Int -> IO (Bool,Bool)
guess a b = do
    putStrLn ("ASK " <> show a <> " " <> show b)
    hFlush stdout
    [x,y] <- getLine <&> words
    pure (x=="yes", y=="yes")
