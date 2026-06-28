import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bits             (clearBit, countTrailingZeros,
                                        popCount, shiftL, testBit)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    let
        arr :: UArray (Int,Int) Int
        arr = listArray ((0,0),(n-1,n-1)) xs

        permanent :: STRef s (M.IntMap Int) -> (Int,Int) -> ST s Int
        permanent memo (ymask,xmask)
            | popCount ymask == 1 = pure (arr ! (countTrailingZeros ymask, countTrailingZeros xmask))
            | otherwise = do
                let h = ymask `shiftL` n + xmask
                s <- readSTRef memo <&> M.lookup h
                case s of
                    Just  r -> pure r
                    Nothing -> do
                        let j  = countTrailingZeros xmask
                        ret <- [0..n-1]
                            & filter (testBit ymask)
                            & mapM (\i -> permanent memo (clearBit ymask i, clearBit xmask j) <&> (* arr ! (i, j)))
                            <&> sum
                        modifySTRef memo (M.insert h ret)
                        pure ret

    print $ runST $ do
        memo <- newSTRef M.empty
        permanent memo ((1 `shiftL` n) - 1, (1 `shiftL` n) - 1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
