import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([l,k,s],xs) <- C.getContents <&> (C.lines >>> (head >>> C.words >>> map readInt) &&& (tail >>> map parseRegistration))
    record k M.empty M.empty [] xs
        & map show
        & unlines
        & putStr

record :: Int -> M.IntMap Int -> M.IntMap Int -> [(Int,Int)] -> [(Int,Int)] -> [Int]
record k regs time comp [] = map snd (sort comp)
record k regs time comp ((i,x):xs) = record k (M.insert i r regs) (M.insert i t time) comp' xs
  where
    comp' | r == k = (t,i) : comp
          | otherwise = comp
    r = 1 + M.findWithDefault 0 i regs
    t = x + M.findWithDefault 0 i time

parseRegistration :: C.ByteString -> (Int,Int)
parseRegistration x = (i,60*m+s)
  where
    Just (i,ms) = C.readInt x
    [m,s] = map readInt (C.split '.' (C.drop 1 ms))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
