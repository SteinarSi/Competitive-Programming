import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (unless, zipWithM_)
import           Control.Monad.ST      (ST)
import           Data.Array            (Array)
import           Data.Array.Base       (UArray, bounds, elems, indices,
                                        listArray, newArray, newArray_,
                                        readArray, writeArray, (!))
import           Data.Array.ST         (STArray, STUArray, runSTArray)
import           Data.Bits             (shiftL)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (range)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

data Destiny = Cycle Int | Walk Int (Int,Int)

main :: IO ()
main = do
    nm:rest <- C.getContents <&> C.lines

    let [n,m] = map (readInt >>> fromIntegral) (C.words nm)
        (grid, queries) = splitAt (fromIntegral n) rest
                & (concatMap C.unpack >>> listArray ((1,1),(n,m)))
                    ***
                  (drop 1 >>> map (C.words >>> map readInt >>> \[x,y,k] -> ((fromIntegral x, fromIntegral y),k)))

    putStr (solve (n,m) grid queries)

solve :: (Int,Int) -> UArray (Int,Int) Char -> [((Int,Int),Int)] -> String
solve (n,m) grid = map (robot >>> uncurry (printf "%d %d")) >>> unlines
  where
    robot :: ((Int,Int),Int) -> (Int,Int)
    robot (u,k) = case destiny ! u of
        Cycle l              -> run highest (hash u) (k `mod` l)
        Walk s v | s < k     -> run highest (hash u) k
                 | otherwise -> robot (v,k-s)

    destiny :: Array (Int,Int) Destiny
    destiny = runSTArray $ do
        dest <- newArray_ (bounds grid)
        seen <- newArray (bounds grid) False
        mapM_ (precompute dest seen) (indices grid)
        pure dest
      where
        precompute :: STArray s (Int,Int) Destiny -> STUArray s (Int,Int) Bool -> (Int,Int) -> ST s ()
        precompute dest seen u = do
            s <- readArray seen u
            unless s $ do
                (path,end) <- walkUntil seen [] u
                let (walk,loop) = span (end/=) path
                unless (null loop) $ mapM_ (flip (writeArray dest) (Cycle (length loop))) loop
                zipWithM_ (\l v -> writeArray dest v (Walk l end)) [1..] (reverse walk)

        walkUntil :: STUArray s (Int,Int) Bool -> [(Int,Int)] -> (Int,Int) -> ST s ([(Int,Int)],(Int,Int))
        walkUntil seen ret u = do
            s <- readArray seen u
            if s
                then pure (reverse ret, u)
                else writeArray seen u True >> walkUntil seen (u:ret) (step u)

    step :: (Int,Int) -> (Int,Int)
    step (y,x) = case grid ! (y,x) of
        '>' -> (y,x+1)
        '<' -> (y,x-1)
        '^' -> (y-1,x)
        'v' -> (y+1,x)

    walk :: (Int,Int) -> [(Int,Int)]
    walk = iterate step

    run :: Int -> Int -> Int -> (Int,Int)
    run s pos 0 = unhash pos
    run s pos k | step <= k = run s (strict ! (pos,s)) (k-step)
                | otherwise = run (s-1) pos k
      where step = 1 `shiftL` s

    strict :: UArray (Int,Int) Int
    strict = elems dp
            & map hash
            & listArray ((hash (1,1),0),(hash (n,m),highest))
      where
        rng = (((1,1),0),((n,m),highest))

        dp :: Array ((Int,Int),Int) (Int,Int)
        dp = listArray rng (map f (range rng))

        f :: ((Int,Int),Int) -> (Int,Int)
        f ((y,x),0) = step (y,x)
        f ((y,x),s) = dp ! (dp ! ((y,x),s-1), s-1)

    hash :: (Int,Int) -> Int
    hash (y,x) = y*m + x

    unhash :: Int -> (Int,Int)
    unhash h = case quotRem h m of
        (q,0) -> (q-1,m)
        (q,r) -> (q,r)

highest :: Int
highest = 15

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
