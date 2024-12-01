import qualified Data.Map as M
import Data.Int (Int16)
import Data.Char (isDigit)
import Control.Monad (liftM)
import Data.List (foldl')
import Debug.Trace


main :: IO ()
main = do
    f <- liftM lines $ readFile "day5-input.txt"
    let points1 = createPoints f False
        map1 = foldl' (\m p -> M.insertWith (+) p (1::Int16) m) M.empty points1
        points2 = createPoints f True
        map2 = foldl' (\m p -> M.insertWith (+) p (1::Int16) m) M.empty points2
    print (length (filter (>=2) (M.elems map1)))
    print (length (filter (>=2) (M.elems map2)))



createPoints :: [String] -> Bool -> [(Int16, Int16)]
createPoints [] _ = []
createPoints (x:xs) diag = let (fx, r1) = takeDropWhile isDigit x
                               (fy, r2) = takeDropWhile isDigit (tail r1)
                               (tx, r3) = takeDropWhile isDigit (drop 4 r2)
                               (ty, []) = takeDropWhile isDigit (tail r3)
                           in  line (read fx, read fy) (read tx, read ty) diag ++ createPoints xs diag
                    -- Linjen over er feil

                    
takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile = takeDropWhile' []
    where takeDropWhile' akk p [] = (reverse akk, [])
          takeDropWhile' akk p (x:xs) | p x = takeDropWhile' (x:akk) p xs
                                      | otherwise = (reverse akk, x:xs)

line :: (Int16, Int16) -> (Int16, Int16) -> Bool -> [(Int16, Int16)]
line (fx, fy) (tx, ty) diag | fx == tx  = zip (repeat fx) (absrange fy ty)
                            | fy == ty  = zip (absrange fx tx) (repeat fy)
                            | diag      = zip (absrange fx tx) (absrange fy ty)
                            | otherwise = []

absrange :: (Enum e, Ord e) => e -> e -> [e]
absrange f t | f <= t    = [f..t]
             | otherwise = [f, pred f..t]