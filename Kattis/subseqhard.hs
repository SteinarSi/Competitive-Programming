import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (solve (M.singleton 0 1) 0 >>> show)
        >>> unlines
        >>> putStr
    )

solve :: M.IntMap Int -> Int -> [Int] -> Int
solve _ _ [] = 0
solve seen c (x:xs) = M.findWithDefault 0 (c'-47) seen + solve (M.insertWith (+) c' 1 seen) c' xs
  where
    c' = c+x

parse :: [C.ByteString] -> [[Int]]
parse []           = []
parse (_:_:xs:xss) = map readInt (C.words xs) : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
