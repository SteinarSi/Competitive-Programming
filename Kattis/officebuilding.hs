import           Control.Arrow            ((>>>), (&&&), (***))
import           Data.Array.Base          (UArray, listArray, (!), elems, assocs)
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.Ix                  (range)
import           Data.Maybe               (fromJust)
import           Data.List                (transpose)
import           Data.Tuple               (swap)

main :: IO ()
main = do
    rc:rest <- C.getContents <&> C.lines

    let [r,c] = C.words rc 
            & map readInt
        (trees, ([s,t],sketch)) = splitAt r rest
            & (concatMap C.words >>> map readInt >>> listArray ((0,0),(r-1,c-1)))
                ***
              ((head >>> (C.words >>> map readInt)) &&& (tail >>> map (C.unpack >>> map ('#'==))))

        best = ((s,t),sketch)
            & iterate (swap *** (transpose >>> map reverse)) 
            & take 4
            & map (\((s',t'),ps) -> listArray ((0,0),(s'-1,t'-1)) (concat ps) & damage trees (r,c) (s',t'))
            & minimum

    elems trees
        & sum
        & subtract best
        & print

damage :: UArray (Int,Int) Int -> (Int,Int) -> (Int,Int) -> UArray (Int,Int) Bool -> Int
damage trees (r,c) (s,t) plan = range ((0,0),(r-s,c-t))
        & map (\(y,x) -> assocs plan
            & filter snd
            & map fst
            & map (\(i,j) -> trees ! (y+i,x+j))
            & sum)
        & minimum

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
