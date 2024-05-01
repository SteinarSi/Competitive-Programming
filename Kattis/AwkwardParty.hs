import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    print (solve M.empty (zip [0..] xs) n)

solve :: M.IntMap Int -> [(Int,Int)] -> Int -> Int
solve _ [] a = a
solve seen ((i,x):xs) a = solve (M.insert x i seen) xs (maybe a ((i-) >>> min a) (M.lookup x seen))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
