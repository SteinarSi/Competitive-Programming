import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (_,r):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    xs
        & filter (uncurry (<))
        & sort
        & invest r
        & subtract r
        & print

invest :: Int -> [(Int,Int)] -> Int
invest r [] = r
invest r ((x,y):xs)
    | r >= x    = invest (r-x+y) xs
    | otherwise = r

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
