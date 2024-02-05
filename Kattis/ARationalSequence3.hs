import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (parse >>> fmap solve >>> format)
        >>> mapM_ C.putStrLn
    )

solve :: Int -> (Int,Int)
solve n = walk (1,1) lr
    where lr = reverse (leftRight n)

walk :: (Int, Int) -> [Bool] -> (Int, Int)
walk (p,q) []         = (p  , q  )
walk (p,q) (True :lr) = walk (p+q, q  ) lr
walk (p,q) (False:lr) = walk (p  , p+q) lr

leftRight :: Int -> [Bool]
leftRight 0 = []
leftRight 1 = []
leftRight n = odd n : leftRight (n `div` 2)

parse :: C.ByteString -> (C.ByteString, Int)
parse s = (a, readInt b)
    where (a:b:_) = C.words s

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

format :: (C.ByteString, (Int,Int)) -> C.ByteString
format (i, (x,y)) = C.unwords [i, C.pack (show x ++ "/" ++ show y)]
