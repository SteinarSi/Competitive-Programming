import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:c:xs <- C.getContents <&> (C.words >>> map readInt)
    let (count, first) = build 0 M.empty M.empty xs
    M.assocs count
        & sortOn (\(x,b) -> (negate b, M.lookup x first))
        & concatMap (uncurry (flip replicate))
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn

build :: Int -> M.IntMap Int -> M.IntMap Int -> [Int] -> (M.IntMap Int, M.IntMap Int)
build i count first [] = (count, first)
build i count first (x:xs) = case M.lookup x count of
    Just b  -> build (i+1) (M.insert x (b+1) count) first xs
    Nothing -> build (i+1) (M.insert x 1 count) (M.insert x i first) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
