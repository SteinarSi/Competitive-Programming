import           Control.Arrow         (first, (&&&), (***), (>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray, STUArray, newListArray,
                                        readArray, writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let (canvases,pegs) = splitAt n rest
            & map (head &&& last) *** last

    putStrLn $ case count canvases pegs of
        Nothing  -> "impossible"
        Just cnt -> runST (newListArray (1,n+1) cnt
                        >>= solve canvases (S.fromList pegs)
                        <&> format)

format :: [Int] -> String
format xs = show (length xs) <> "\n" <> unwords (map show xs)

solve :: forall s. [(Int,Int)] -> S.IntSet -> STUArray s Int Int -> ST s [Int]
solve canvases pegs count = sol (zip [1..] canvases)
    where
        sol :: [(Int,(Int,Int))] -> ST s [Int]
        sol [] = pure []
        sol ((i,(l,r)):xs) = do
            right <- readArray count (i+1) <&> ((>=2) >>> bool r (r-1))
            needs <- readArray count i <&> (2-)
            let add = [right,right-1..]
                        & filter (`S.notMember` pegs)
                        & take needs
            when (map (snd >>> fst) (take 1 xs) == take 1 add) (modifyArray count (i+1) succ)
            (add <>) <$> sol xs

count :: [(Int,Int)] -> [Int] -> Maybe [Int]
count [] _ = Just [0]
count ((l,r):xs) ps | overlaps > 2 = Nothing
                    | otherwise    = (overlaps:) <$> count xs ps'
    where
        (overlaps,ps') = span (<r) ps
            & first (filter (l<=) >>> length >>> (+ bool 0 1 ([r] == take 1 ps')))

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
