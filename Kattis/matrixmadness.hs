import           Control.Arrow      ((>>>))
import           Data.Array.ST      (modifyArray, newArray, runSTArray)
import           Data.Array.Unboxed (Array, UArray, assocs, listArray, (!))
import           Data.Bool          (bool)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))

main :: IO ()
main = do
    n:m:xs <- getContents <&> (words >>> map read)

    let
        grid :: UArray (Int,Int) Int
        grid = listArray ((1,1),(n,m)) xs

        positions :: Array Int [(Int,Int)]
        positions = runSTArray $ do
            ret <- newArray (1,15) []
            mapM_ (\(p,v) -> modifyArray ret v (p:)) (assocs grid)
            pure ret

    assocs grid
        & all (\((x,y),v) -> positions ! v
            & filter (/=(x,y))
            & filter (\(x',y') -> abs (x-x') + abs (y-y') <= v)
            & null)
        & bool "INVALID" "VALID"
        & putStrLn
