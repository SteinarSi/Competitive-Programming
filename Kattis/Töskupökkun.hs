import           Control.Arrow         ((&&&), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> (head >>> last)
                &&&
            (tail >>> map (sort >>> (\[x,y,z] -> (x,y,z))))
        >>> uncurry solulu
        >>> maybe "Omogulegt!" show
        >>> putStrLn
    )

solulu :: Int -> [(Int,Int,Int)] -> Maybe Int
solulu t xs = bin (maximum (map (\(_,y,_) -> y) xs)) (maximum (map (\(_,_,z) -> z) xs))
  where
    bin :: Int -> Int -> Maybe Int
    bin lo hi | lo > hi   = Nothing
              | otherwise = case time mi 0 xs of
                  Nothing            -> bin (mi+1) hi
                  Just c | c > t     -> bin (mi+1) hi
                         | lo == hi  -> Just lo
                         | otherwise -> bin lo mi
      where
        mi = (lo+hi) `div` 2

    time :: Int -> Int -> [(Int,Int,Int)] -> Maybe Int
    time k c [] = Just c
    time k c ((x,y,z):xs)
        | z <= k    = time k (c+x) xs
        | y <= k    = time k (c+z) xs
        | otherwise = Nothing

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
