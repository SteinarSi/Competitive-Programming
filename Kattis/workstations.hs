import           Control.Arrow            ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.List                (sort)
import           Data.Maybe               (fromJust)

main :: IO ()
main = do
    (n,m):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    parse m [] [] xs
        & uncurry (assign m)
        & (n-)
        & print

parse :: Int -> [Int] -> [Int] -> [(Int,Int)] -> ([Int],[Int])
parse _ locks arrivals [] = (sort locks, sort arrivals)
parse m locks arrivals ((a,s):xs) = parse m (a+s:locks) (a:arrivals) xs

assign :: Int -> [Int] -> [Int] -> Int
assign _ _ [] = 0
assign m [] (a:as) = 1 + assign m [] as
assign m (l:ls) (a:as) | a < l     = 1 + assign m (l:ls) as
                       | a > l+m   = assign m ls (a:as)
                       | otherwise = assign m ls as

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
