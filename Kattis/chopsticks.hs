import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))
    putStrLn $ if count S.empty xs >= n
        then "yes"
        else "no"

count :: S.IntSet -> [(Int,Int)] -> Int
count set [] = 0
count set ((t,l):xs)
    | S.member h set = 1 + count (S.delete h set) xs
    | otherwise      =     count (S.insert h set) xs
  where
    h = t*1000+l

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
