import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST)
import           Data.Array.Base       (newArray, readArray, writeArray, (!))
import           Data.Array.ST         (STUArray, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [k]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> drop 1 >>> map readInt))

    let prefs = runSTUArray $ do
            ret <- newArray ((1,1),(k,k)) 0
            mapM_ (preferences ret (S.fromList [1..k])) xs
            pure ret

    [1..k]
        & find (\x -> all (\y -> x == y || prefs ! (x,y) > 0) [1..k])
        & maybe "impossible" show
        & putStrLn

preferences :: STUArray s (Int,Int) Int -> S.IntSet -> [Int] -> ST s ()
preferences prefs unseen [] = pure ()
preferences prefs unseen (x:xs) = do
    S.foldl' (\r y -> do
        readArray prefs (x,y) >>= (succ >>> writeArray prefs (x,y))
        readArray prefs (y,x) >>= (pred >>> writeArray prefs (y,x))
        r
        ) (pure ()) unseen
    preferences prefs (S.delete x unseen) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
