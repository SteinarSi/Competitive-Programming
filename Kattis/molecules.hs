import           Control.Arrow         ((&&&), (***), (>>>))
import           Control.Monad         (forM_)
import           Data.Array            (Array)
import           Data.Array.Base       (MArray (newArray), UArray, elems,
                                        listArray, readArray, writeArray, (!))
import           Data.Array.ST         (runSTArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust, mapMaybe)

main :: IO ()
main = do
    (n,m):rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let ((coords, fixed), graph) = splitAt n rest
            & ((map (\(x,y) -> bool (Just (fromIntegral x, fromIntegral y)) Nothing ((x,y)==(-1,-1))) >>> listArray (1,n))
                    &&&
               (map ((-1,-1)/=) >>> listArray (1,n)))
                ***
              parseGraph n

    coords
        & spring n graph fixed 450
        & format
        & putStr

spring :: Int -> Array Int [Int] -> UArray Int Bool -> Int -> Array Int (Maybe (Double,Double)) -> Array Int (Maybe (Double,Double))
spring _ _ _ 0 coords         = coords
spring n graph fixed k coords = [1..n]
        & map initial
        & listArray (1,n)
        & spring n graph fixed (k-1)
    where
        initial :: Int -> Maybe (Double,Double)
        initial i | fixed ! i = coords ! i
                  | otherwise = graph ! i
                        & mapMaybe (coords!)
                        & middle

format :: Array Int (Maybe (Double,Double)) -> String
format = elems >>> map f >>> unlines
    where
        f Nothing      = "-1 -1"
        f (Just (x,y)) = show x <> " " <> show y

parseGraph :: Int -> [(Int,Int)] -> Array Int [Int]
parseGraph n xs = runSTArray $ do
        arr <- newArray (1,n) []
        forM_ xs $ \(a,b) -> do
            modifyArray arr a (b:)
            modifyArray arr b (a:)
        pure arr

middle :: [(Double,Double)] -> Maybe (Double,Double)
middle xs = do
    x <- mean (map fst xs)
    y <- mean (map snd xs)
    Just (x,y)

mean :: [Double] -> Maybe Double
mean [] = Nothing
mean xs = Just (sum xs / fromIntegral (length xs))

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
