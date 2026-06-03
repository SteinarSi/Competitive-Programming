import           Control.Arrow         ((>>>))
import           Control.Monad         (forM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)


main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let arr :: UArray Int Int
        arr = listArray (1,n) xs

    print $ sum $ runST $ do
            seen <- newArray (1,n) False :: ST s (STUArray s Int Bool)
            forM [1..n] $ \x -> do
                s <- readArray seen x
                if s || arr ! x == x
                    then pure 0
                    else do
                        let cycle = x : takeWhile (/=x) (drop 1 (iterate (arr!) x))
                        forM_ cycle (flip (writeArray seen) True)
                        pure (length cycle + 1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
