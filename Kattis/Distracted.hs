import           Control.Arrow         (first, second, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    [n,l]:rest <- C.getContents <&> (C.lines >>> map C.words)

    map (head &&& last) rest
        & splitAt (readInt n)
        & first (map (second C.head) >>> M.fromList)
        & uncurry (solve '0')
        & putChar

solve :: Char -> M.Map C.ByteString Char -> [(C.ByteString,C.ByteString)] -> Char
solve q status [] = q
solve q status ((x,y):xs) = case (status M.! x, status M.! y) of
    ('m','u') -> '1'
    ('m','?') -> solve '?' (M.insert y 'm' status) xs
    ('?','u') -> solve '?' (M.insert x 'u' status) xs
    ('?','?') -> solve '?' status xs
    _         -> solve  q  status xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
