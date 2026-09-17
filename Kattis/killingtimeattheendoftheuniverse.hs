import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, listArray, (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (cy,dy):(n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let
        travels :: Array Int (Int,Int)
        travels = listArray (1,n) xs

        graph :: Array Int [Int]
        graph = listArray (1,n) (map (\(_,t) -> filter ((travels!) >>> fst >>> (>=t)) [1..n]) xs)

        ordering = runST $ do
            seen <- newArray (1,n) False
            [1..n]
                & filter ((travels!) >>> fst >>> (>=cy))
                & findMaybeM (order seen (n,[]))

        order :: STUArray s Int Bool -> (Int,[Int]) -> Int -> ST s (Maybe [Int])
        order seen (1,rs) u = pure (Just (reverse (u:rs)))
        order seen (r,rs) u = do
            writeArray seen u True
            next <- filterM (readArray seen >>> (<&> not)) (graph ! u)
            ret <- case next of
                []  -> pure Nothing
                v:_ -> order seen (r-1,u:rs) v
            writeArray seen u False
            pure ret

        age :: Int -> [(Int,Int)] -> Int
        age prev [] = dy-prev
        age prev ((f,t):xs)
            | f < prev  = minBound
            | otherwise = f-prev + age t xs

    print $ if n <= 0
        then dy-cy
        else fromJust ordering
            & map (travels!)
            & age cy

findMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findMaybeM p []     = pure Nothing
findMaybeM p (x:xs) = p x >>= maybe (findMaybeM p xs) (Just >>> pure)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
