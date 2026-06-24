import           Control.Arrow         (second, (>>>))
import           Control.Monad         (unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (MArray (newArray, newArray_), STUArray,
                                        readArray, runSTArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (Array, Ix, UArray, array, assocs,
                                        bounds, range, (!))
import           Data.Bits             (shiftR)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

-- | Each vertex is mapped to the representative of its strongly connected component.
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

invertGraph :: Array Int [Int] -> Array Int [Int]
invertGraph graph = runSTArray $ do
    ret <- newArray (bounds graph) []
    mapM_ (\(u,vs) -> mapM_ (\v -> modifyArray ret v (u:)) vs) (assocs graph)
    pure ret

powmod :: Integer -> Integer -> Integer -> Integer
powmod _ 0 _ = 1
powmod x n m
    | odd n     = (u'*x) `mod` m
    | otherwise = u'
  where
    u = powmod x (n `shiftR` 1) m
    u' = (u*u) `mod` m

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readDouble :: C.ByteString -> Double
readDouble s
    | C.head s == '-' = negate (readDouble (C.tail s))
    | C.length r1 <= 1 = fromIntegral int
    | otherwise = fromIntegral int + float
  where
    (int, r1)
        | C.head s == '.' = (0, s)
        | otherwise = fromJust (C.readInt s)
    Just (dec, r2) = C.readInt (C.tail r1)
    float
        | C.length r1 <= 1 = 0
        | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
