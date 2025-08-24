import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> last >>> readInt)
        >>> deposit S.empty
        >>> print
    )

deposit :: S.Set [Int] -> [Int] -> Int
deposit stacks [] = S.size stacks
deposit stacks (x:xs) = case S.lookupGT [x] stacks of
    Nothing -> deposit (S.insert [x] stacks) xs
    Just ys -> deposit (S.insert (x:ys) (S.delete ys stacks)) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
