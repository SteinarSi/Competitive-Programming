import           Control.Arrow         (second, (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (depths M.empty M.empty >>> deepest)
        >>> C.unlines
        >>> C.putStr
    )

deepest :: M.IntMap [C.ByteString] -> C.ByteString
deepest = M.toAscList >>> last >>> snd >>> sort >>> C.unwords

depths :: M.IntMap [C.ByteString] -> M.IntMap Int -> [(Int,(Int,C.ByteString))] -> M.IntMap [C.ByteString]
depths ret parents []              = ret
depths ret parents ((i,(p,x)):xs)  = depths (M.insertWith (<>) d [x] ret) (M.insert i d parents) xs
    where d = M.findWithDefault (-1) p parents + 1

parse :: [C.ByteString] -> [[(Int,(Int,C.ByteString))]]
parse [] = []
parse (n:xs) = splitAt (readInt n) xs
    & (map (C.readInt >>> fromJust >>> second (C.drop 1)) >>> zip [0..] ) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
