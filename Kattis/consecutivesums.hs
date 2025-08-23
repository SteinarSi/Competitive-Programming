import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.List             (intercalate, minimumBy)
import           Data.Maybe            (fromJust, listToMaybe, mapMaybe)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map (readInt >>> solve)
        >>> unlines
        >>> putStr
    )

solve :: Int -> String
solve n = [2..]
        & takeWhile (\b -> b*b <= 2*n)
        & mapMaybe check
        & listToMaybe
        & maybe "IMPOSSIBLE" format
    where
        check :: Int -> Maybe (Int,Int)
        check b | r == 0 && (odd q && even b || even q && odd b) = Just (a,a+b-1)
                | otherwise = Nothing
            where
                (q,r) = quotRem (2*n) b
                a = (q - b + 1) `div` 2

        format :: (Int,Int) -> String
        format (a,b) = show n <> " = " <> intercalate " + " (map show [a..b])

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
