import           Control.Arrow      ((>>>))
import           Control.Monad      (zipWithM_)
import           Data.Array.ST      (newArray, readArray, runSTArray,
                                     writeArray)
import           Data.Array.Unboxed (Array, UArray, listArray, (!))
import           Data.Function      ((&))
import           Data.Maybe         (fromJust)

main :: IO ()
main = getContents >>= (
            lines
        >>> init
        >>> parse
        >>> map solve
        >>> unlines
        >>> putStr
    )

solve :: (Int,Int,String,[String]) -> String
solve (p,s,board,xs) = play (map (,0) (take p ['1'..])) (zip [1..] xs)
  where
    play :: [(Char,Int)] -> [(Int,String)] -> String
    play _ [] = "No player won after " <> show (length xs) <> " cards."
    play ((player,i):ps) ((_,[]):css) = play (ps <> [(player,i)]) css
    play ((player,i):ps) ((j,c:cs):css)
        | u == l+1 || i' == s = "Player " <> [player] <> " won after " <> show j <> " cards."
        | otherwise           = play ((player,i'):ps) ((j,cs):css)
      where
        (ixs,l) = indices ! c
        u = upperBound 0 (l+1)
        i' = ixs ! u

        upperBound :: Int -> Int -> Int
        upperBound lo hi
            | (hi-lo <= 1) = hi
            | i < ixs ! mi = upperBound lo mi
            | otherwise    = upperBound mi hi
          where
            mi = (lo + hi) `div` 2

    indices :: Array Char (UArray Int Int, Int)
    indices = (runSTArray $ do
            ret <- newArray ('A','Z') []
            zipWithM_ (\i x -> readArray ret x >>= ((i:) >>> writeArray ret x)) [1..] board
            pure ret)
        & fmap (\is -> (listArray (1,length is) (reverse is), length is))

parse :: [String] -> [(Int,Int,String,[String])]
parse [] = []
parse (psc:board:xs) = (p, s, board, ys) : parse zs
  where
    [p,s,c] = map read (words psc)
    (ys,zs) = splitAt c xs
