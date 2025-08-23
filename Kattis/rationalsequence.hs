import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (parse >>> fmap smart >>> format)
        >>> mapM_ C.putStrLn
    )

smart :: (Int, Int) -> (Int, Int)
smart (p, q) | p < q = (q, q - p)
             | otherwise = let (d, r) = quotRem p q
                           in  (q, q - r + d * q)

parse :: C.ByteString -> (C.ByteString, (Int, Int))
parse s = (a, (readInt b, readInt (C.tail c)))
    where (a:bc:_) = C.words s
          (b, c)   = C.span isDigit bc

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

format :: (C.ByteString, (Int,Int)) -> C.ByteString
format (i, (x,y)) = C.unwords [i, C.pack (show x ++ "/" ++ show y)]
