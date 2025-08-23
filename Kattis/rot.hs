import           Control.Arrow      (second, (&&&), (>>>))
import           Data.Array.ST      (newArray, runSTUArray, writeArray)
import           Data.Array.Unboxed (elems)
import           Data.Bool          (bool)
import           Data.Char          (isSpace)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Data.List          (dropWhileEnd, transpose)

main :: IO ()
main = do
    (rc:xs,k) <- getContents <&> (lines >>> init &&& last)

    let (wholes,half) = quotRem (read k) 90

    iterate (transpose >>> map reverse) xs
        & (!!wholes)
        & bool id diamond (half==45)
        & unlines
        & putStr

diamond :: [String] -> [String]
diamond xss = elems arr
    & chunksOf (r+c-1)
    & map (dropWhileEnd isSpace)
  where
    r = length xss
    c = length (head xss)
    arr = runSTUArray $ do
        ret <- newArray ((0,0),(r+c-2,r+c-2)) ' '
        sequence_ [writeArray ret (y+x,r-y-1+x) l | (y,xs) <- zip [0..] xss, (x,l) <- zip [0..] xs]
        pure ret

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
