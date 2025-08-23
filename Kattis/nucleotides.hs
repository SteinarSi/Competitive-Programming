import           Control.Arrow         ((&&&), (>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (genome,xs) <- C.getContents <&> (C.lines >>> head &&& (drop 2 >>> map (C.words >>> map readInt)))

    let [as,ts,gs,cs] = map (counter genome) "ATGC"
        solve i j = map fst (sortOn (snd >>> negate) [
                ('A',as ! j - as ! (i-1)),
                ('T',ts ! j - ts ! (i-1)),
                ('G',gs ! j - gs ! (i-1)),
                ('C',cs ! j - cs ! (i-1))
            ])

    map (\[i,j] -> solve i j) xs
        & unlines
        & putStr

counter :: C.ByteString -> Char -> UArray Int Int
counter genome x = C.foldl (\(r:rs) y -> bool r (r+1) (y==x) : r : rs) [0] genome
    & reverse
    & listArray (0,C.length genome)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
