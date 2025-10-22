import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

data Transaction = DRIP Int Int | NDRIP Int
    deriving (Read)

main :: IO ()
main = do
    ([s,m,_],xs) <- C.getContents <&> (C.lines >>> (head >>> C.words >>> map (C.readInt >>> fromJust >>> fst)) &&& (drop 1 >>> map (C.unpack >>> read)))
    putStrLn (solve s m xs)

solve :: Int -> Int -> [Transaction] -> String
solve s m [] = show s <> "\n" <> show m
solve s m (DRIP d p : xs) = solve (s+b) (m+e) xs 
  where
    (b,e) = (s * d) `quotRem` p
solve s m (NDRIP p : xs) = solve (s+b) m' xs 
  where
    (b,m') = m `quotRem` p
