import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = do
    (days,[d,h]) <- C.getContents <&> (C.lines >>> init &&& (last >>> C.words >>> map readInt))

    days
        & (`choose` d)
        & map (\subset -> [0..23]
            & map (\i -> length (filter ((`C.index` i) >>> (=='.')) subset))
            & sortOn Down
            & take h
            & sum
            & fromIntegral
            & (/ fromIntegral (d*h)))
        & maximum
        & print

choose :: [a] -> Int -> [[a]]
choose _  0     = [[]]
choose xs 1     = map pure xs
choose (x:xs) i = map (x:) (choose xs (i-1)) <> choose xs i
choose [] _     = []

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
