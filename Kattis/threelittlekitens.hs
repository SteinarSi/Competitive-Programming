import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (UArray, assocs, inRange, listArray, (!))
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Data.List          (find)
import           Data.Maybe         (listToMaybe)
import           Text.Printf        (printf)

main :: IO ()
main = do
    h:w:xs <- getContents <&> lines

    let
        rng = ((1,1),(read h, read w))

        grid :: UArray (Int,Int) Char
        grid = listArray rng (concat xs)

        solve :: Char -> String
        solve c = printf "%d %d %d %d %d %d" my mx sy sx ty tx
          where
            Just ((sy,sx),_) = find (\((y,x),k) -> c==k && length (neighbours (y,x)) == 1) (assocs grid)
            path = follow (-1,-1) (sy,sx)
            (ty,tx) = last path
            (my,mx) = minimum path

            follow :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
            follow prev curr = neighbours curr
                & filter (/=prev)
                & listToMaybe
                & maybe [curr] (follow curr >>> (curr:))

            neighbours :: (Int,Int) -> [(Int,Int)]
            neighbours (y,x) = [(y',x') | y' <- [y-1..y+1], x' <- [x-1..x+1], inRange rng (y',x'), (y,x) /= (y',x'), grid ! (y',x') == c]

    putStrLn (solve 'R')
    putStrLn (solve 'G')
    putStrLn (solve 'B')
