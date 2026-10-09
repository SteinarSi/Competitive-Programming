import           Control.Arrow      ((>>>))
import           Control.Monad      (when)
import           Control.Monad.ST   (ST)
import           Data.Array.ST      (STUArray, modifyArray, newArray,
                                     runSTUArray, writeArray)
import           Data.Array.Unboxed (UArray, elems, listArray, (!))
import           Data.Function      ((&))
import           Data.Functor       ((<&>))

main :: IO ()
main = do
    [n,m]:xs <- getContents <&> (lines >>> map (words >>> map read))

    let
        friends :: UArray (Int,Int) Bool
        friends = runSTUArray $ do
            ret <- newArray ((1,1),(n,n)) False
            mapM_ (\[a,b] -> writeArray ret (a,b) True >> writeArray ret (b,a) True) xs
            pure ret

        ans = runSTUArray $ do
            ret <- newArray (0,n) 0
            groups ret 0 [] n
            pure ret

        groups :: STUArray s Int Int -> Int -> [Int] -> Int -> ST s ()
        groups cnt f fs 0 = modifyArray cnt f succ
        groups cnt f fs i = do
            groups cnt f fs (i-1)
            when (not (any ((,i) >>> (friends !)) fs)) $ groups cnt (f+1) (i:fs) (i-1)

    elems ans
        & map show
        & unwords
        & putStrLn
