import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (STUArray, modifyArray, newArray,
                                        readArray, runSTArray, writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (0,n) S.empty
            forM_ (init xs) $ \[a,b] -> do
                modifyArray ret a (S.insert b)
                modifyArray ret b (S.insert a)
            writeArray ret n (S.fromAscList [0..n-1])
            pure ret

        dfs :: STUArray s Int Bool -> Int -> [Int] -> ST s (Maybe [Int])
        dfs seen curr [] = mapM (readArray seen) [0..n-1] <&> (and >>> bool Nothing (Just []))
        dfs seen curr (x:xs)
            | S.notMember x (graph ! curr) = mapM (readArray seen) (S.toList (graph ! curr)) <&> (and >>> bool Nothing (Just (x:xs)))
            | otherwise = do
                s <- readArray seen x
                if s
                    then pure Nothing
                    else do
                        writeArray seen x True
                        dfs seen x xs >>= maybe (pure Nothing) (dfs seen curr)

    putStrLn $ runST $ do
        seen <- newArray (0,n) False
        dfs seen n (last xs) <&> (maybe False null >>> bool "NO" "YES")

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
