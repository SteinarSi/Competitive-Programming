import           Control.Arrow         (second, (>>>))
import           Data.Array            (Array, listArray, range, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines

    let (syllables,poem) = splitAt (readInt n) rest

    putStrLn $ if and (zipWith (haiku syllables) [5,7,5] poem)
        then "haiku"
        else "come back next year"

haiku :: [C.ByteString] -> Int -> C.ByteString -> Bool
haiku syllables s xs = dp ! (s,0)
    where
        n = C.length xs
        rng = ((0,0),(s,n))

        dp :: Array (Int,Int) Bool
        dp = listArray rng (map f (range rng))

        f :: (Int,Int) -> Bool
        f (0,i) = i == n
        f (r,i) | i > n = False
                | i == n = r == 0
                | xs `C.index` i == ' ' = dp ! (r,i+1)
                | otherwise = syllables
            & filter (`C.isPrefixOf` C.drop i xs)
            & any (\ss -> dp ! (r-1,i + C.length ss))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
