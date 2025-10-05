import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [_,xs,trees] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    print (scatter trees S.empty xs)

scatter :: [Int] -> S.IntSet -> [Int] -> Int
scatter trees used [] = 0
scatter trees used (x:xs)
    | S.member t used = 1 + scatter trees used xs
    | otherwise       = scatter trees (S.insert t used) xs
  where
    t = minimumBy (compare `on` (\i -> (abs (x-i), i))) trees

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
