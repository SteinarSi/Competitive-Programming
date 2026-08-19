import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:b:xs <- C.getContents <&> (C.words >>> map readInt)

    let ans = map (subtract k) xs
            & scanl1 (+)
            & zip [0..]
            & solve b (M.singleton 0 (-1))

    putStrLn $ case ans of
        [] -> "-1"
        ys -> let (i,j) = minimum ys
              in  show i <> " " <> show j

solve :: Int -> M.IntMap Int -> [(Int,Int)] -> [(Int,Int)]
solve b seen []         = []
solve b seen ((i,x):xs) = case M.lookup (x-b) seen of
    Nothing ->           solve b (M.insertWith min x i seen) xs
    Just  j -> (j+1,i) : solve b (M.insertWith min x i seen) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
