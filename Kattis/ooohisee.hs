import           Control.Arrow         ((&&&), (>>>))
import           Data.Array            (range)
import           Data.Array.Base       (UArray, indices, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([r,c],rest) <- C.getContents <&> (
                C.lines
            >>> (head >>> (C.words >>> map readInt))
                &&&
                (tail >>> C.concat >>> C.unpack)
            )
    let grid = listArray ((1,1),(r,c)) rest :: UArray (Int,Int) Char
        spots = range ((2,2),(r-1,c-1))
            & filter ((grid!) >>> (=='0'))
            & filter (\(y,x) -> all ((grid!) >>> (=='O')) [(y',x') | y' <- [y-1..y+1], x' <- [x-1..x+1], y'/=y || x'/=x])

    putStrLn $ case spots of
        []      -> "Oh no!"
        [(y,x)] -> show y <> " " <> show x
        xs      -> "Oh no! " <> show (length xs) <> " locations"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
