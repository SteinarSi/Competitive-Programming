import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [_,xs,ys] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let xx = S.fromList xs
        yy = S.fromList ys

    [
        filter (`S.notMember` yy) xs,
        filter (`S.notMember` xx) ys
        ]
        & map (map show >>> unwords)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
