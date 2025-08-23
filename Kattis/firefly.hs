import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (sort, sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))


main :: IO ()
main = do
    n:h:xs <- C.getContents <&> (C.words >>> map readInt)
    let (mites, tites) = everyOther ([],[]) xs
            & sort *** (sortOn Down >>> map (\x -> h - x + 1))
        todo = [1..h]
        test = crashes (n `div` 2) todo (mites,tites)
        best = minimum test
        count = filter (==best) test & length

    putStrLn (show best <> " " <> show count)

crashes :: Int -> [Int] -> ([Int],[Int]) -> [Int]
crashes crash []     _                   = []
crashes crash (x:xs) (m:ms, ts) | x > m  =         crashes (crash-1) (x:xs) (ms, ts)
crashes crash (x:xs) (ms, t:ts) | x >= t =         crashes (crash+1) (x:xs) (ms, ts)
crashes crash (x:xs) (ms,ts)             = crash : crashes crash     xs     (ms, ts)

everyOther :: ([Int],[Int]) -> [Int] -> ([Int],[Int])
everyOther (l,r) []       = (l,r)
everyOther (l,r) [x]      = (x:l,r)
everyOther (l,r) (x:y:xs) = everyOther (x:l,y:r) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
