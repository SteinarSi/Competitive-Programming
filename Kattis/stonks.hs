import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> \[s,p,t] -> (s,[(readInt p, readInt t)]))
        >>> M.fromListWith (<>)
        >>> M.elems
        >>> map (sortOn snd >>> map fst >>> \(x:xs) -> stonks x xs)
        >>> sum
        >>> print
    )

stonks :: Int -> [Int] -> Int
stonks _ []     = 0
stonks l (x:xs) = max (x-l) (stonks (min l x) xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
