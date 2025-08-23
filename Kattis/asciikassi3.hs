import           Control.Arrow    (second, (>>>))
import           Control.Monad    (forM_)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (STUArray, getElems, newArray, readArray,
                                   writeArray)
import           Data.Bool        (bool)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

main :: IO ()
main = do
    [h,w,d] <- getLine <&> (words >>> map read)

    putStr $ runST $ do
        canvas <- newArray ((1,1),(h+d-1,w+d-1)) ' ' :: ST s (STUArray s (Int,Int) Char)

        let write x ix = do
                y <- readArray canvas ix
                if y /= ' ' && y /= x
                    then writeArray canvas ix 'x'
                    else writeArray canvas ix x

            hori (sy,sx,tx) = mapM_ ((sy,) >>> write '-') [sx+1..tx-1]
            vert (sy,sx,ty) = mapM_ ((,sx) >>> write '|') [sy+1..ty-1]
            diag (sy,sx) = mapM_ (\i -> write '/' (sy+i,sx+i)) [1..d-1]

        vert (d,d,h+d-1)
        hori (d,d,w+d-1)
        diag (1,1)
        mapM_ vert [(1,1,h),(1,w,h),(d,w+d-1,h+d-1)]
        mapM_ hori [(1,1,w),(h,1,w),(h+d-1,d,w+d-1)]
        mapM_ diag [(1,w),(h,1),(h,w)]
        forM_ [(1,1),(1,w),(d,d),(d,w+d-1),(h,1),(h,w),(h+d-1,d),(h+d-1,w+d-1)] (flip (writeArray canvas) '+')

        getElems canvas <&> (
                chunksOf (w+d-1)
            >>> reverse
            >>> map rstrip
            >>> unlines)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

rstrip :: String -> String
rstrip = reverse >>> dropWhile (==' ') >>> reverse
