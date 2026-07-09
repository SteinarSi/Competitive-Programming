import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sortOn)
import           Data.Maybe            (fromMaybe)

main :: IO ()
main = do
    xs <- C.getLine
    runST (do
        ret <- newArray ('a','z') 0 :: ST s (STUArray s Char Int)
        mapM_ (\x -> readArray ret x >>= (succ >>> writeArray ret x)) (C.unpack xs)
        getAssocs ret)
            & filter (snd >>> (>0))
            & sortOn (snd >>> negate)
            & distribute
            & fromMaybe "IMPOSSIBLE"
            & putStrLn

distribute :: [(Char,Int)] -> Maybe [Char]
distribute []           = Just []
distribute ((_,0):xs)   = distribute xs
distribute ((x,1):xs)   = (x:) <$> distribute xs
distribute [_]          = Nothing
distribute ((x,c):y:xs) = (x:) <$> distribute (y : insert (x,c-1) xs)

insert :: (Char,Int) -> [(Char,Int)] -> [(Char,Int)]
insert a [] = [a]
insert (a,c) ((x,n):xs)
    | c >= n    = (a,c) : (x,n) : xs
    | otherwise = (x,n) : insert (a,c) xs
