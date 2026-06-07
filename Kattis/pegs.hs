import           Control.Arrow     ((>>>))
import           Control.Monad     (guard)
import           Control.Monad.ST  (ST, runST)
import           Data.Bits         (popCount, shiftL, (.&.))
import           Data.Function     ((&))
import           Data.Functor      ((<&>))
import qualified Data.IntSet       as S
import           Data.STRef.Strict (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = do
    start <- getContents <&> (lines
            >>> zipWith (\y -> words >>> zipWith (\x c -> ((y,x),c=="X")) [0..]) [0..]
            >>> concat
            >>> filter snd
            >>> map (fst >>> bit)
            >>> sum
        )
    print $ runST (newSTRef S.empty >>= flip solve start)

solve :: STRef s S.IntSet -> Int -> ST s Int
solve ref curr = do
    seen <- readSTRef ref
    modifySTRef ref (S.insert curr)
    let next = do
            from@(y,x) <- positions
            guard (bit from .&. curr > 0)
            move@(dy,dx) <- moves
            let over = (y+dy,x+dx)
                to   = (y+dy+dy,x+dx+dx)
            guard (inRange to && bit to .&. curr == 0 && bit over .&. curr > 0)
            let result = curr - bit from - bit over + bit to
            guard (S.notMember result seen)
            pure (curr - bit from - bit over + bit to)
    if null next
        then pure (popCount curr)
        else mapM (solve ref) next <&> minimum

moves :: [(Int,Int)]
moves = [(0,-1),(0,1),(-1,-1),(-1,0),(1,0),(1,1)]

positions :: [(Int,Int)]
positions = [(y,x) | y <- [0..4], x <- [0..y]]

bit :: (Int,Int) -> Int
bit (y,x) = 1 `shiftL` (5*y + x)

inRange :: (Int,Int) -> Bool
inRange (y,x) = 0 <= y && y <= 4 && 0 <= x && x <= y
