import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([n,k],stats) <- C.getContents <&> (
                C.lines
            >>> map (C.words >>> map readInt)
            >>> head &&& (tail >>> zip [1..])
        )

    [0..2]
        & map (\s -> stats
            & sortOn (snd >>> (!!s) >>> negate)
            & map fst
            & take k
            & S.fromList)
        & S.unions
        & S.size
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
