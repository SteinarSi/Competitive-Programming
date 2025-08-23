import           Control.Arrow    ((***), (>>>))
import           Control.Monad.ST (ST, runST)
import           Data.Array.ST    (STArray, getElems, newArray, readArray,
                                   writeArray)
import           Data.Bool        (bool)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

data Instruction = CLEAR Int | SET Int | OR Int Int | AND Int Int
    deriving Read

main :: IO ()
main = getContents >>= (
            lines
        >>> parse
        >>> map (\xs -> runST (newArray (0,31) Nothing >>= (`solve` xs)))
        >>> unlines
        >>> putStr
    )

solve :: STArray s Int (Maybe Bool) -> [Instruction] -> ST s String
solve memory [] = getElems memory <&> (reverse >>> map (maybe '?' (bool '0' '1')))
solve memory (x:xs) = do
    case x of
        CLEAR i -> writeArray memory i (Just False)
        SET i -> writeArray memory i (Just True)
        OR i j -> do
            p <- readArray memory i
            q <- readArray memory j
            writeArray memory i $ case (p,q) of
                (Just a, Just b) -> Just (a || b)
                (Just True,_)    -> Just True
                (_, Just True)   -> Just True
                _                -> Nothing
        AND i j -> do
            p <- readArray memory i
            q <- readArray memory j
            writeArray memory i $ case (p,q) of
                (Just a, Just b) -> Just (a && b)
                (Just False,_)   -> Just False
                (_, Just False)  -> Just False
                _                -> Nothing
    solve memory xs

parse :: [String] -> [[Instruction]]
parse [] = []
parse [_] = []
parse (n:xs) = splitAt (read n) xs
        & map read
            ***
          parse
        & uncurry (:)
