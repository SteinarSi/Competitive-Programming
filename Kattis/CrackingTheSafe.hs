{-# LANGUAGE Strict #-}

import           Control.Arrow            ((>>>))
import           Control.Monad            (filterM)
import           Control.Monad.ST         (ST, runST)
import           Data.Array.Base          (STUArray, readArray, writeArray, newArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.Maybe               (fromJust)

type State = (Int,Int,Int,Int,Int,Int,Int,Int,Int)

main :: IO ()
main = do
    [a,b,c,d,e,f,g,h,i] <- C.getContents <&> (C.words >>> map readInt)

    print $ runST $ do
        seen <- newArray (0, 4^9) False
        crack 0 seen [(a,b,c,d,e,f,g,h,i)]

crack :: Int -> STUArray s Int Bool -> [State] -> ST s Int
crack ret seen [] = pure (-1)
crack ret seen xss | (0,0,0,0,0,0,0,0,0) `elem` xss = pure ret
                   | otherwise = concatMap moves xss
                        & filterM (hash >>> (\h -> readArray seen h >>= \s -> writeArray seen h True >> pure (not s)))
                        & (>>= crack (ret+1) seen)

moves :: State -> [State]
moves (a,b,c,d,e,f,g,h,i) = [
        (sm a, sm b, sm c, sm d, e, f, sm g, h, i),
        (sm a, sm b, sm c, d, sm e, f, g, sm h, i),
        (sm a, sm b, sm c, d, e, sm f, g, h, sm i),
        (sm a, b, c, sm d, sm e, sm f, sm g, h, i),
        (a, sm b, c, sm d, sm e, sm f, g, sm h, i),
        (a, b, sm c, sm d, sm e, sm f, g, h, sm i),
        (sm a, b, c, sm d, e, f, sm g, sm h, sm i),
        (a, sm b, c, d, sm e, f, sm g, sm h, sm i),
        (a, b, sm c, d, e, sm f, sm g, sm h, sm i)
    ]

hash :: State -> Int
hash (a,b,c,d,e,f,g,h,i) = a + b*4 + c*16 + d*64 + e*256 + f*1024 + g*4096 + h*16384 + i*65536

sm :: Int -> Int
sm 3 = 0
sm x = x+1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
