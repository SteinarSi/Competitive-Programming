import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (STArray, STUArray, getElems, newArray,
                                        readArray, runSTArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust, mapMaybe)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let force = runSTArray $ do
            ret <- newArray (1,n) Nothing
            mapM_ (\[a,b] -> when (a<0) (writeArray ret (-a) (Just b))) xs
            pure ret
        graph = runSTArray $ do
            ret <- newArray (1,n) []
            mapM_ (\[a,b] -> readArray ret (abs a) >>= ((b:) >>> writeArray ret (abs a))) xs
            pure ret
        end = fmap fromJust $ runSTArray $ do
            ret <- newArray (1,n) Nothing
            mapM_ (ending force ret) [1..n]
            pure ret

    print $ runST $ do
        seen <- newArray (1,n) False
        stop <- newArray (1,n) False
        solve graph force end seen stop 1
        getElems stop <&> (filter id >>> length)

solve :: Array Int [Int] -> Array Int (Maybe Int) -> Array Int (Maybe Int) -> STUArray s Int Bool -> STUArray s Int Bool -> Int -> ST s ()
solve graph force end seen stop u = do
    s <- readArray seen u
    if s
        then pure ()
        else do
            writeArray seen u True
            graph ! u
                & mapMaybe (end !)
                & mapM_ (flip (writeArray stop) True)
            case force ! u of
                Nothing -> writeArray stop u True
                Just  v -> solve graph force end seen stop v

ending :: Array Int (Maybe Int) -> STArray s Int (Maybe (Maybe Int)) -> Int -> ST s (Maybe Int)
ending force end u = case force ! u of
    Nothing -> writeArray end u (Just (Just u)) >> pure (Just u)
    Just  v -> do
        r <- readArray end u
        case r of
            Just  e -> pure e
            Nothing -> do
                writeArray end u (Just Nothing)
                q <- ending force end v
                writeArray end u (Just q)
                pure q

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
