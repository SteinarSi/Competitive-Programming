import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> concatMap (C.words >>> map readInt >>> \[m,n] -> solve m n)
        >>> C.concat
    )

solve :: Int -> Int -> [C.ByteString]
solve m n = map (memo!) [1+m..n]
    & (C.pack (show ((n*pred n) `div` 2 - (m*pred m) `div` 2) <> "\n"):)

memo :: Array Int C.ByteString
memo = listArray (1,2000) (map f [1..2000])
  where
    f a = [1,3..(a-2)*2+1]
        & map (\b -> C.pack (show a <> " " <> show b <> " " <> show a <> " " <> show (b+1)))
        & C.unlines

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
