import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Ord              (comparing)
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    n <- readInt <$> C.getLine
    solve (replicate n 0)

solve :: [Int] -> IO ()
solve xs = do
    let Interval _ i j = kadane (Interval (-1) undefined undefined) (Interval (-1) undefined undefined) (zip [1..] xs)
    putStrLn (show i <> " " <> show j)
    hFlush stdout
    xs <- C.getLine <&> (C.words >>> map readInt)
    p <- C.getLine <&> readInt
    when (p < 70) (solve xs)

kadane :: Interval -> Interval -> [(Int,Int)] -> Interval
kadane best curr [] = best
kadane best curr ((j,1):xs) = kadane best (curr { value = value curr - 1, end = j }) xs
kadane best curr ((j,0):xs) = kadane (max best next) next xs
  where
    next = max (curr { value = value curr + 1, end = j }) Interval { value = 1, start = j, end = j }

data Interval = Interval {
    value :: Int,
    start :: Int,
    end   :: Int
} deriving (Eq,Show)

instance Ord Interval where
    compare = comparing value

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
