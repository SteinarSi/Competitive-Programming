import           Control.Arrow         (second, (>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTArray, writeArray)
import           Data.Array.Unboxed    (Array, UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    nrhms:rest <- C.getContents <&> (C.lines >>> map C.words)
    let
        [n,_,h,m,s] = map readInt nrhms
        (cities,roads) = splitAt n rest
            & second (map (map readInt))
        rng = (0,n-1)

        vs :: UArray Int Int
        vs = listArray rng (map (\[_,_,v] -> readInt v) cities)

        names :: Array Int C.ByteString
        names = listArray rng (map (\[_,n,_] -> n) cities)

        graph :: Array Int [(Int,Int)]
        graph = fmap sort $ runSTArray $ do
            ret <- newArray rng []
            forM_ roads $ \[f,t,d] -> do
                readArray ret f >>= (((d,t):) >>> writeArray ret f)
                readArray ret t >>= (((d,f):) >>> writeArray ret t)
            pure ret

        solve :: STUArray s Int Int -> [Int] -> Int -> Int -> ST s ([Int],Int)
        solve prev ret t curr = do
            writeArray prev curr t
            next <- graph ! curr
                    & filter (\(d,v) -> t + d + vs ! v <= m)
                    & filterM (\(d,v) -> readArray prev v <&> ((+h) >>> (<=t+d)))
            case next of
                []      -> pure (reverse ret, t)
                (d,v):_ -> solve prev (v:ret) (t+d+vs!v) v

        (cs,t) = runST $ do
            prev <- newArray rng minBound
            solve prev [s] (vs ! s) s

    C.putStrLn (C.unwords (map (names!) cs))
    print t

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
