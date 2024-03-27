import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> zip [0..]
        >>> filter (fst >>> odd)
        >>> mapM_ (
                snd
            >>> C.words
            >>> map (C.readInt >>> fromJust >>> fst)
            >>> friday 0
            >>> print
        )
    )

friday :: Int -> [Int] -> Int
friday _ [] = 0
friday d (x:xs) | x >= 13 && d == 0 = succ next
                | otherwise         =      next
    where next = friday ((d + x) `mod` 7) xs
