import           Control.Arrow         (second, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    ([p,q,n],xs) <- C.getContents <&> (C.lines
            >>> map (C.words >>> map readInt)
            >>> head &&& (tail
                >>> map (head &&& (last >>> pure))
                >>> M.fromListWith (<>)
                >>> M.assocs
                >>> map (second sort)))
    solve (p,q) 1 xs
        & concat
        & putStrLn

solve :: (Int,Int) -> Int -> [(Int,[Int])] -> [String]
solve (p,q) a [] = ["0"]
solve (p,q) a ((y,xs):xss) = row 0 xs <> solve (p,q) (c+1) xss
  where
    c | null xss  = p
      | otherwise = y

    row :: Int -> [Int] -> [String]
    row _ []     = []
    row r [_]    = [printf "%d %d %d %d\n" a (r+1) c q]
    row r (d:ds) = printf "%d %d %d %d\n" a (r+1) c d : row d ds

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
