import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (findIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([f],xs) <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> map readInt >>> pairs) >>> last &&& init)
    xs
        & findIndex (\[u,v,w] -> (area2 u v f + area2 v w f + area2 w u f) == area2 u v w)
        & maybe "-1" show
        & putStrLn

area2 :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
area2 (x1,y1) (x2,y2) (x3,y3) = abs (x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2))

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs (x:y:xs) = (x,y) : pairs xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
