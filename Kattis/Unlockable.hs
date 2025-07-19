import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:a:bs <- C.getContents <&> (C.words >>> map readInt)

    bs
        & S.fromList
        & S.toAscList
        & map (solutions a)
        & sum
        & print

solutions :: Int -> Int -> Int
solutions a b | r /= 0    = 0
              | otherwise = 1 + solutions a y
  where
    (y,r) = quotRem b a

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
