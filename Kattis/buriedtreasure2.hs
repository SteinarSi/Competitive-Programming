import           Control.Arrow         ((>>>))
import           Control.Monad         (unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (MArray (newArray, newArray_), STUArray,
                                        readArray, runSTArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (Array, Ix, UArray, array, assocs,
                                        bounds, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let outgraph = runSTArray $ do
            ret <- newArray (-m,m) []
            mapM_ (\[a,b] -> modifyArray ret (-a) (b:) >> modifyArray ret (-b) (a:)) xs
            pure ret
        ingraph = runSTArray $ do
            ret <- newArray (-m,m) []
            mapM_ (\[a,b] -> modifyArray ret a (-b:) >> modifyArray ret b (-a:)) xs
            pure ret

        scc = kosarajuSCC outgraph ingraph
        reasonable = all (\u -> scc ! u /= scc ! (-u)) [1..m]

    putStrLn $ if reasonable
        then "YES"
        else "NO"

kosarajuSCC :: Array Int [Int] -> Array Int [Int] -> UArray Int Int
kosarajuSCC outgraph ingraph = runSTUArray $ do
    comp <- newArray_ (bounds outgraph)
    seen <- newArray (bounds outgraph) False
    mapM_ (\u -> assign comp seen u u) (postorder outgraph)
    pure comp
  where
    assign :: STUArray s Int Int -> STUArray s Int Bool -> Int -> Int -> ST s ()
    assign comp seen root u = do
        s <- readArray seen u
        unless s $ do
            writeArray seen u True
            writeArray comp u root
            mapM_ (assign comp seen root) (ingraph ! u)

postorder :: Array Int [Int] -> [Int]
postorder outgraph = runST $ do
    seen <- newArray (bounds outgraph) False
    mapM (visit seen) (range (bounds outgraph)) <&> (reverse >>> concat)
  where
    visit :: STUArray s Int Bool -> Int -> ST s [Int]
    visit seen u = do
        s <- readArray seen u
        if s
            then pure []
            else do
                writeArray seen u True
                mapM (visit seen) (outgraph ! u) <&> (reverse >>> concat >>> (u:))

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
