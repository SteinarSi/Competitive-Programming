import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, newArray_,
                                        readArray, runSTUArray, writeArray)
import           Data.Array.Unboxed    ((!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:c1:c2:c3:xs <- C.getContents <&> (C.words >>> map readInt)

    let n = sum xs
        yes = runSTUArray $ do
            ret <- newArray (1,n+3) True
            scanl1 (+) xs
                & init
                & (<>[n+1..n+3])
                & mapM_ (flip (writeArray ret) False)
            pure ret
        xyz@[x,y,z] = zipWith min [c1,c2,c3] [n, (n+1)`div` 2, (n+2) `div` 3]

        solve :: STUArray s (Int,Int,Int,Int) Bool -> (Int,Int,Int,Int) -> ST s Bool
        solve seen (i,a,b,c) = case compare i n of
            GT -> pure False
            EQ -> pure True
            LT -> do
                s <- readArray seen (i,a,b,c)
                if s
                    then pure False
                    else do
                        writeArray seen (i,a,b,c) True
                        [
                                (yes ! (i+1) && a > 0, (i+1,a-1,b,c)),
                                (yes ! (i+2) && b > 0, (i+2,a,b-1,c)),
                                (yes ! (i+3) && c > 0, (i+3,a,b,c-1))
                            ]
                            & filter fst
                            & anyM (snd >>> solve seen)

    putStrLn $ runST (newArray ((0,0,0,0),(n,x,y,z)) False >>= flip solve (0,x,y,z) <&> bool "NO" "YES")

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p []     = pure False
anyM p (x:xs) = p x >>= bool (anyM p xs) (pure True)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
