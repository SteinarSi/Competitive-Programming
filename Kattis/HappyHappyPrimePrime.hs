{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow               ((&&&), (***), (>>>))
import           Control.Monad               (forM_, (>=>))
import           Data.Array.Unboxed          (UArray, listArray, (!))
import           Data.Bool                   (bool)
import qualified Data.ByteString.Char8       as C
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import           Data.Maybe                  (fromJust)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Word                   (Word8)

main :: IO ()
main = do
    memo <- MV.replicate 10000 0
    C.getContents >>= (
                C.lines
            >>> tail
            >>> mapM_ ((
                    (<>)
                        &&&
                    (C.words
                        >>> last
                        >>> C.readInt
                        >>> fromJust
                        >>> fst
                        >>> isHappyPrime memo
                        >>> fmap (bool " NO" " YES" >>> C.pack))
                >>> uncurry fmap)
                >=> C.putStrLn)
        )

isHappyPrime :: MV.IOVector Word8 -> Int -> IO Bool
isHappyPrime memo m | not (isPrime m) = pure False
                    | otherwise       = isHappy' [] m
    where
        isHappy' :: [Int] -> Int -> IO Bool
        isHappy' qs q = do
            s <- MV.read memo q
            if | s == 2 || q == 1 -> forM_ qs (flip (MV.write memo) 2) >> pure True
               | s == 1           -> pure False
               | otherwise        -> MV.write memo q 1 >> isHappy' (q:qs) (squareDigits q)

squareDigits :: Int -> Int
squareDigits 0 = 0
squareDigits p = quotRem p 10
    & squareDigits *** (^2)
    & uncurry (+)

isPrime :: Int -> Bool
isPrime = (arr !)
    where
        (t, f) = (True, False)

        arr :: UArray Int Bool
        arr = listArray (0,10000) [f,f,t,t,f,t,f,t,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f]
