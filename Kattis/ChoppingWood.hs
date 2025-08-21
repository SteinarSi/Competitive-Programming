import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':xs <- C.getContents <&> (C.words >>> map readInt)
    solve (n'+1) xs
        & maybe "Error\n" (map show >>> unlines)
        & putStr

solve :: Int -> [Int] -> Maybe [Int]
solve n xs
    | n /= last xs = Nothing
    | otherwise = runST $ do
        cnt <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
        mapM_ (\x -> readArray cnt x >>= (succ >>> writeArray cnt x)) xs
        int <- getAssocs cnt <&> (filter (snd >>> (==0)) >>> map fst >>> S.fromList)
        process cnt int xs
  where
    process :: STUArray s Int Int -> S.IntSet -> [Int] -> ST s (Maybe [Int])
    process cnt int [] = pure (Just [])
    process cnt int (v:vs) = case S.minView int of
        Nothing -> pure Nothing
        Just (u,int') -> do
            c <- readArray cnt v
            writeArray cnt v (c-1)
            let int'' | c == 1 = S.insert v int'
                      | otherwise = int'
            ((u:) <$>) <$> process cnt int'' vs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
