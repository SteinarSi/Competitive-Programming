import           Control.Arrow      (second, (&&&), (>>>))
import           Control.Monad      (forM_, when)
import           Control.Monad.ST   (ST, runST)
import           Data.Array.ST      (STUArray, newArray, readArray, runSTArray,
                                     runSTUArray, writeArray)
import           Data.Array.Unboxed ((!))
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import qualified Data.IntMap.Strict as M
import           Data.Ratio         (denominator, numerator, (%))

main :: IO ()
main = do
    [n,m] <- getLine <&> (words >>> map read)

    let peak = floor (sqrt (fromIntegral n))
        dp = runSTUArray $ do
            ret <- newArray ((0,0),(peak,n)) (0::Int)
            writeArray ret (0,n) 1
            forM_ [n,n-1..1] $ \r -> do
                forM_ [0..peak] $ \d -> do
                    c <- readArray ret (d,r)
                    when (c > 0) $ [floor (sqrt (fromIntegral d))..]
                        & map (\s -> (s,s*s-d))
                        & dropWhile (snd >>> (<1))
                        & takeWhile (snd >>> (<= min r m))
                        & map (second (r-))
                        & mapM_ (\i -> readArray ret i >>= ((c+) >>> (`mod` base) >>> writeArray ret i))
            pure ret

        counts = [0..peak]
            & map (fromIntegral &&& ((,0) >>> (dp!) >>> fromIntegral))
            & filter (snd >>> (>0))

    putStrLn $ case counts of
        [] -> "-1"
        cs -> let (p,q) = (numerator &&& denominator) (sum (map (uncurry (*)) counts) % sum (map snd counts))
                  (s,_,_) = gcdExt q base
              in  show ((p * s) `mod` base)

base :: Integral a => a
base = 1000000007

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b = (t, s - q * t, g)
  where
    (q, r) = a `quotRem` b
    (s, t, g) = gcdExt b r
