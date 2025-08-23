import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let pollution :: Int -> Int
        pollution s = sum (map (\(x,c) -> c * abs (s-x)) xs)

        bin :: Int -> Int -> Int
        bin lo hi | lo >= hi  = lo
                  | p1 <= p2  = bin lo (mi2-1)
                  | otherwise = bin (mi1+1) hi
          where
            diff = (hi-lo) `div` 3
            mi1 = lo + diff
            mi2 = hi - diff

            p1 = pollution mi1
            p2 = pollution mi2

    xs
        & map fst
        & minimum &&& maximum
        & uncurry bin
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
