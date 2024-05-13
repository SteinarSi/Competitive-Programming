import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (catMaybes, fromJust, listToMaybe)

main :: IO ()
main = C.getContents >>= (
            readInt
        >>> throw 3 0 []
        >>> maybe "impossible" (map format >>> unlines >>> init)
        >>> putStrLn
    )

format :: (Int, Int) -> String
format (1, x) = "single " ++ show x
format (2, x) = "double " ++ show x
format (3, x) = "triple " ++ show x

throw :: Int -> Int -> [(Int,Int)] -> Int -> Maybe [(Int,Int)]
throw r c ret n = case compare c n of
    GT -> Nothing
    EQ -> Just ret
    LT | r == 0    -> Nothing
       | otherwise -> (do
            s <- [1..20]
            t <- [1..3]
            pure (throw (r-1) (c+s*t) ((t,s):ret) n)
        )
        & catMaybes
        & listToMaybe

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
