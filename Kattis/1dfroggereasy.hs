import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, listArray, newArray,
                                        readArray, writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:s:m:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn $ runST $ do
        seen <- newArray (1,n) False
        play (listArray (1,n) xs) n m seen 0 s

play :: UArray Int Int -> Int -> Int -> STUArray s Int Bool -> Int -> Int -> ST s String
play board n m seen steps s
    | s < 1 = pure ("left\n" <> show steps)
    | s > n = pure ("right\n" <> show steps)
    | board ! s == m = pure ("magic\n" <> show steps)
    | otherwise = do
        r <- readArray seen s
        if r
            then pure ("cycle\n" <> show steps)
            else do
                writeArray seen s True
                play board n m seen (steps+1) (s + board ! s)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
