import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (C.lines >>> parse >>> map solve >>> C.unlines)

solve :: [Int] -> C.ByteString
solve xs = C.unlines (C.pack (show k) : distribute cnt)
  where
    k = maximum (map (snd >>> length) cnt)

    cnt :: [(Int,[Int])]
    cnt = M.assocs (M.fromListWith (<>) (map (id &&& pure) xs))

distribute :: [(Int,[Int])] -> [C.ByteString]
distribute [] = []
distribute cnt = foldr (\(k,v) (g,c) -> case v of
        []     -> (g,c)
        [x]    -> (x:g,c)
        (x:xs) -> (x:g,(k,xs):c)
    ) ([],[]) cnt
    & (map (show >>> C.pack) >>> C.unwords) *** distribute
    & uncurry (:)

parse :: [C.ByteString] -> [[Int]]
parse []         = []
parse [_]        = []
parse (_:xs:xss) = map readInt (C.words xs) : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
