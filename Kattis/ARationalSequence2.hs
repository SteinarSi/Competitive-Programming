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

solve :: (Int, Int) -> Int
solve xy = base + offset
    where
        lr     = leftRight xy
        base   = 2 ^ length lr
        offset = sum [ 2^i | (i,b) <- zip [0..] lr, b ]

leftRight :: (Int, Int) -> [Bool]
leftRight (1, 1) = []
leftRight (x, y) | x > y     = True : leftRight (x-y, y)
                 | otherwise = False : leftRight (x, y-x)

parse :: C.ByteString -> (C.ByteString, (Int, Int))
parse s = (a, (readInt b, readInt (C.tail c)))
    where (a:bc:_) = C.words s
          (b, c)   = C.span isDigit bc

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

format :: (C.ByteString, Int) -> C.ByteString
format (i, n) = C.unwords [i, C.pack (show n)]
