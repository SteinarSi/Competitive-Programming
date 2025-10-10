import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)
import           Data.Function         ((&))
import           Data.List             (sortBy)
import           Data.Maybe            (fromJust)

data Class = LOWER | MIDDLE | UPPER deriving (Eq,Ord,Read,Show)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> concatMap solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: [(C.ByteString,[Class])] -> [C.ByteString]
solve = sortBy cmp >>> map fst >>> (<>[C.replicate 30 '='])

cmp :: (C.ByteString,[Class]) -> (C.ByteString,[Class]) -> Ordering
cmp (x,xs) (y,ys) = foldr (<>) (compare x y) (zipWith compare ys xs)

parse :: [C.ByteString] -> [[(C.ByteString,[Class])]]
parse [] = []
parse (n:xs) = splitAt (readInt n) xs
    & map (C.words >>> parse') *** parse
    & uncurry (:)
  where
    parse' [name,cs,_] = (C.init name, take 10 (map (C.unpack >>> map toUpper >>> read) (reverse (C.split '-' cs)) <> repeat MIDDLE))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
