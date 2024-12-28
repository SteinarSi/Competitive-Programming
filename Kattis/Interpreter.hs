import           Control.Arrow         ((>>>))
import           Control.Monad         (join, liftM2, zipWithM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray(..), STUArray, readArray, writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.words >>> map readInt)

    print $ runST $ do
        ram <- newArray (0,999) 0
        reg <- newArray (0,9) 0
        zipWithM_ (\i x -> writeArray ram i x) [0..] xs
        interpret ram reg 0 0

interpret :: STUArray s Int Int -> STUArray s Int Int -> Int -> Int -> ST s Int
interpret ram reg steps head = do
        instr <- readArray ram head <&> parseInstruction

        let jump = interpret ram reg (steps+1)
            next = jump (head+1)

        case instr of
            (1,_,_) -> pure (steps+1)
            (2,d,n) -> writeArray reg d n                                                   >> next
            (3,d,n) -> modifyArray reg d (add n)                                            >> next
            (4,d,n) -> modifyArray reg d (mult n)                                           >> next
            (5,d,s) -> readArray reg s >>= writeArray reg d                                 >> next
            (6,d,s) -> readArray reg s >>= (add  >>> modifyArray reg d)                     >> next
            (7,d,s) -> readArray reg s >>= (mult >>> modifyArray reg d)                     >> next
            (8,d,a) -> readArray reg a >>= readArray ram >>= writeArray reg d               >> next
            (9,s,a) -> join (liftM2 (writeArray ram) (readArray reg a) (readArray reg s))   >> next
            (0,d,s) -> readArray reg s >>= ((==0) >>> bool (readArray reg d >>= jump) next)

add :: Int -> Int -> Int
add = (+) >>> (>>> (`mod` 1000))

mult :: Int -> Int -> Int
mult = (*) >>> (>>> (`mod` 1000))

parseInstruction :: Int -> (Int,Int,Int)
parseInstruction x = (x `div` 100, (x `div` 10) `mod` 10, x `mod` 10)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
