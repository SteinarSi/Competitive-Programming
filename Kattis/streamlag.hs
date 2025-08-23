import           Control.Arrow         ((>>>), (&&&))
import           Data.Array.Base       (UArray, (!))
import           Data.Array.ST         (newArray_, runSTUArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let times = runSTUArray $ do
            arr <- newArray_ (1,n)
            mapM_ (\(t:p:_) -> writeArray  arr p t) xs
            pure arr

    print $ stream n times 1 1

stream :: Int -> UArray Int Int -> Int -> Int -> Int
stream n times time next | next > n  = 0
                         | delay > 0 = delay + stream n times (time+delay+1) (next+1)
                         | otherwise = stream n times (time+1) (next+1)
    where 
        delay = max 0 (times ! next - time)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
