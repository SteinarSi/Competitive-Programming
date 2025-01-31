import           Control.Arrow         (first, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [t,_]:ts:rest <- C.getContents <&> (C.lines >>> map C.words)
    let types = map readInt ts
        versions = map (head &&& (last >>> readInt)) rest
        (latest,queries) = rest
            & map (head &&& (last >>> readInt))
            & splitAt (readInt t)
            & first M.fromList
    split types queries
        & map (map (\(name,v) -> latest M.! name - v)
            >>> sum
            >>> show
            >>> C.pack)
        & C.unlines
        & C.putStr

split :: [Int] -> [a] -> [[a]]
split [] _ = []
split (t:ts) xs = let (a,b) = splitAt t xs
                  in  a : split ts b

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
