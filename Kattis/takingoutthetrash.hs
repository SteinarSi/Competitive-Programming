import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)
    map (,1) xs
        & M.fromListWith (+)
        & trash k 0
        & print

trash :: Int -> Int -> M.IntMap Int -> Int
trash k ret bags | M.null bags = ret
                 | otherwise   = trash k (ret+1) bags''
    where
        (x,bags') = case M.lookupMax bags of
            Nothing     -> error "bruh"
            Just (x',1) -> (x', M.deleteMax bags)
            Just (x',c) -> (x', M.insert x' (c-1) bags)
        bags'' = case M.lookupLE (k-x) bags' of
                Nothing    -> bags'
                Just (y,1) -> M.delete y bags'
                Just (y,c) -> M.insert y (c-1) bags'

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
