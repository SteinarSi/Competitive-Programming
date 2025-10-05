import           Control.Arrow         ((>>>))
import           Data.Array            (Array, assocs, listArray, (!))
import           Data.Array.ST         (newArray, readArray, runSTArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Foldable         (maximumBy)
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let graph = runSTArray $ do
            g <- newArray (1,n) []
            mapM_ (\[u,v] -> readArray g u >>= ((v:) >>> writeArray g u)) xs
            pure g

        dp :: Array Int Int
        dp = listArray (1,n) (map len [1..n])

        len :: Int -> Int
        len u = graph ! u
            & map ((dp!) >>> succ)
            & (1:)
            & maximum

        backtrack :: Int -> [Int]
        backtrack u = case graph ! u of
            [] -> [u]
            ys -> u : backtrack (maximumBy (compare `on` (dp!)) ys)

        (s,l) = maximumBy (compare `on` snd) (assocs dp)

    print l
    putStrLn (unwords (map show (backtrack s)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
