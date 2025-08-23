import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.IntSet           (IntSet, empty, fromList, toAscList,
                                        union)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    w:p:xs <- fmap (C.words >>> map readInt) C.getContents
    (0:xs ++ [w])
        & combs
        & toAscList
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn

combs :: [Int] -> IntSet
combs []     = empty
combs (x:xs) = fromList (map (subtract x) xs) `union` combs xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
