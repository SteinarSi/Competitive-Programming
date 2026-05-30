import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (_,m):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))
    sortOn (\(t,g) -> (t,-g)) xs
        & lastMinute m
        & print

lastMinute :: Int -> [(Int,Int)] -> Int
lastMinute _ [] = 0
lastMinute m ((t,g):xs) | t <= m = g + lastMinute (m-t) xs
                        | otherwise = 0

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
