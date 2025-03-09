import           Control.Arrow ((>>>))
import           Data.Char     (digitToInt)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import qualified Data.IntSet   as S
import           Data.List     (foldl1')
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    k:_:xs <- getContents <&> words

    let codes = xs
            & map (pick 0 (read k))
            & foldl1' S.intersection
            & S.toAscList
            & map (show >>> \xs -> replicate (read k - length xs) '0' <> xs)

    putStr (unlines (show (length codes) : codes))

pick :: Int -> Int -> String -> S.IntSet
pick ret 0 _      = S.singleton ret
pick ret k ""     = S.empty
pick ret k (x:xs) = pick ret k xs <> pick (10*ret + digitToInt x) (k-1) xs
