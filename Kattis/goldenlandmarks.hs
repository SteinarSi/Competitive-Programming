import           Control.Arrow         ((>>>))
import           Control.Monad         (ap)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.lines >>> map C.words)
    let coords = init xs
            & map (\[name,x,y] -> (name,(readInt x, readInt y)))
            & M.fromList
    last xs
        & map (coords M.!)
        & ap (zipWith (\(x1,y1) (x2,y2) -> abs (x1-x2) + abs (y1-y2))) tail
        & sum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
