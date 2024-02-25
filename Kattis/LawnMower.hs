import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    w <- C.getLine <&> (C.words >>> last >>> parse >>> (`div` 2))
    when (w /= 0) $ do
        let p = C.getLine <&> (C.words >>> map (parse >>> (\x -> (x-w,x+w))))
        hori <- p
        vert <- p
        putStrLn $ if solve [(0,75*10^scale)] hori && solve [(0,100*10^scale)] vert
            then "YES"
            else "NO"
        main

solve :: [(Integer,Integer)] -> [(Integer,Integer)] -> Bool
solve [] _ = True
solve ints [] = False
solve ints (cut:cuts) = solve (concatMap (mow cut) ints) cuts
    where
        mow :: (Integer,Integer) -> (Integer,Integer) -> [(Integer,Integer)]
        mow (x,y) (a,b) | b < x = [(a,b)]
                        | a > y = [(a,b)]
                        | x <= a && b <= y = []
                        | a <= x && b <= y = [(a,x-1)]
                        | a >= x && b >= y = [(y+1,b)]
                        | a <= x && b >= y = [(a,x-1), (y+1,b)]
                        | otherwise = error "bruhhhhh"

scale :: Int
scale = 7

-- | parses doubles, and scales them so they can accurately be represented as integers
parse :: C.ByteString -> Integer
parse x = readInteger (a <> C.replicate scale '0') + rest
    where (a:bs) = C.split '.' x
          rest | null bs  = 0
               | otherwise = readInteger (b <> C.replicate (scale-C.length b) '0')
          b = C.concat bs

readInteger :: C.ByteString -> Integer
readInteger = C.readInt >>> fromJust >>> fst >>> fromIntegral

