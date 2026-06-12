import           Control.Arrow         ((&&&), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Ratio            ((%))

main :: IO ()
main = do
    p1:p2:_:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))
    let angle ((x3,y3),(x4,y4))
            | x4 < x3 = angle ((x4,y4),(x3,y3))
            | y3==y4 = Nothing
            | otherwise = Just ((x4-x3) % (y4-y3))
    pairs rest
        & map (angle >>> (==angle (p1,p2)) >>> bool "YES" "NO")
        & unlines
        & putStr

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs (x:y:xs) = (x,y) : pairs xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
