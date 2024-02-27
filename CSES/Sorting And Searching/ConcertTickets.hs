import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.IntMap.Strict    (IntMap, delete, empty, insert,
                                        insertWith, lookupLE)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    C.getLine
    ts <- readInts <&> foldr (\t -> insertWith (+) t 1) empty
    cs <- readInts
    solulu ts cs & mapM_ print

solulu :: IntMap Int -> [Int] -> [Int]
solulu _ []      = []
solulu ts (c:cs) = case lookupLE c ts of
    Just (t, 1) ->  t : solulu (delete t ts) cs
    Just (t, x) ->  t : solulu (insert t (x-1) ts) cs
    Nothing     -> -1 : solulu ts cs

readInts :: IO [Int]
readInts = C.getLine <&> (C.words >>> map (C.readInt >>> fromJust >>> fst))
