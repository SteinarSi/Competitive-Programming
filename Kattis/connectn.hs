import           Control.Arrow         ((***), (>>>))
import           Data.Array.Unboxed    (UArray, assocs, inRange, indices,
                                        listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    x:y:n':rest <- C.getContents <&> C.words

    let rng = ((1,1),(readInt x, readInt y))
        n = readInt n'

        grid :: UArray (Int,Int) Char
        grid = listArray rng (map C.head rest)

        ans = assocs grid
            & find (\((i,j),c) -> c /= 'O' && any (\(dy,dx) ->
                        iterate ((dy+)***(dx+)) (i,j)
                    & take n
                    & takeWhile (inRange rng)
                    & takeWhile ((grid!) >>> (==c))
                    & length
                    & (>=n)
                ) dirs)
            <&> snd
    putStrLn $ case ans of
        Nothing  -> "NONE"
        Just 'R' -> "RED WINS"
        Just 'B' -> "BLUE WINS"

dirs :: [(Int,Int)]
dirs = [(-1,0),(-1,1),(0,1),(1,1)]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
