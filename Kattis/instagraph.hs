import           Control.Arrow         (second, (>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (MArray (..), STArray, getAssocs,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (maximumBy)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let cs = runST $ do
            g <- newArray (1,n) S.empty :: ST s (STArray s Int S.IntSet)
            forM_ xs $ \[u,v] -> do
                us <- readArray g u
                if S.member v us
                    then readArray g u >>= (S.delete v >>> writeArray g u)
                    else readArray g v >>= (S.insert u >>> writeArray g v)
            getAssocs g <&> map (second S.size)

    maximumBy (compare `on` (\(u,c) -> (c,-u))) cs
        & uncurry (printf "%d %d\n")

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
