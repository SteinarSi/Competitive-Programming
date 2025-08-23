import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs:_:qs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let (set,psum) = map (,1) xs
            & M.fromListWith (+)
            & M.toAscList
            & (map fst >>> S.fromAscList)
                &&&
              (prefixSum 0 >>> M.fromAscList)

        below :: Int -> Int
        below = (`S.lookupLE` set) >>> maybe 0 (psum M.!)

    qs
        & map (\[a,b] -> show (below b - below (a-1)))
        & unlines
        & putStr

prefixSum :: Int -> [(Int,Int)] -> [(Int,Int)]
prefixSum p []         = []
prefixSum p ((x,c):xs) = (x,c+p) : prefixSum (c+p) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
