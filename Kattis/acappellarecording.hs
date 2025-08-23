import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:d:xs <- C.getContents <&> (C.words >>> map readInt)
    print (solve d (S.fromList xs))

solve :: Int -> S.IntSet -> Int
solve d set = case S.minView set of
    Nothing     -> 0
    Just (x,xs) -> 1 + solve d (S.dropWhileAntitone (<=x+d) xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
