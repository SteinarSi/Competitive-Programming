import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:t:xs <- C.getContents <&> (C.words >>> map readInt)
    let sorted = sort (zip xs [1..])
    putStr $ case distribute t (map (fst >>> (,[])) sorted) of
        Nothing -> "IMPOSSIBLE\n"
        Just rs -> zip (map snd sorted) rs
            & sort
            & map (snd >>> reverse >>> map show >>> unwords)
            & unlines

distribute :: Int -> [(Int,[Int])] -> Maybe [[Int]]
distribute t xs@((x,_):_)
    | all (fst >>> (<=x+1)) zrs = Just (map (\(x,xs) -> replicate x 1 <> xs) next)
    | x == 0 = mapM (\(x,xs) -> if x <= t then Just ([x,x-1..1] <> xs) else Nothing) xs
    | t == 1 = Nothing
    | otherwise = distribute (t-1) next
  where
    (yrs,zrs) = span (fst >>> (==x)) xs
    next = map (\(z,zs) -> (z-1,t:zs)) yrs <> map (\(z,zs) -> (z-2,t:t:zs)) zrs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
