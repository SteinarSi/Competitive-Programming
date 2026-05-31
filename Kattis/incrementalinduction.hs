import           Control.Arrow         ((>>>))
import           Control.Monad         (zipWithM_, (>=>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getElems, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':xss <- C.getContents <&> C.lines

    let n = readInt n'
        losses = runST $ do
            cnt <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
            zipWithM_ (\i xs -> zipWithM_ (\j x -> do
                    let ix = bool i j (x == '1')
                    c <- readArray cnt ix
                    writeArray cnt ix (c+1)
                    ) [1..] (C.unpack xs)) [2..] xss
            getElems cnt

    losses
        & sort
        & induction 0 0 0
        & print

induction :: Int -> Int -> Int -> [Int] -> Int
induction ret _ _ [] = ret
induction ret curr i (x:xs) = induction (max ret next) next (i+1) xs
  where
    next = curr + (x-i)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
