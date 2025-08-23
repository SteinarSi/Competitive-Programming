import           Control.Arrow              ((&&&), (>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Function              ((&))
import           Data.Functor               ((<&>))
import           Data.Maybe                 (fromJust)

main :: IO ()
main = do
    s:t:p:_ <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let (a@(x1,y1), b@(x2,y2)) = safe p s
        (e@(x5,y5), d@(x4,y4)) = safe p t
        c | x2 == x4 && abs(y2 - y4) > inf = [(inf, 0)]
          | y2 == y4 && abs(x2 - x4) > inf = [(0, inf)]
          | otherwise                      = []
        points = [a,b] ++ c ++ [d,e]

    points
        & map (\(x,y) -> show x <> " " <> show y)
        & (show (length points) :)
        & map C.pack
        & C.unlines
        & C.putStr

safe :: (Int,Int) -> (Int,Int) -> ((Int,Int), (Int,Int))
safe (px,py) (x,y) | y <= py   = ((x   , -inf), (0    , -inf))
                   | x <= px   = ((-inf, y   ), (-inf , 0   ))
                   | y >= py   = ((x   , inf ), (0    , inf ))
                   | otherwise = ((inf , y   ), (inf  , 0   ))

inf :: Int
inf = 10^7

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
