import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Prelude       hiding (Either (..))

data Instruction = Forward | Left | Right
    deriving (Read,Eq,Show,Enum)

main :: IO ()
main = do
    [x,y] <- fmap (words >>> map read) getLine
    getLine
    instructions <- fmap (lines >>> map read) getContents

    let solulu = do
            i <- [0..length instructions-1]
            let (a,b:bs) = splitAt i instructions
            r <- filter (/=b) [Forward .. Right]
            guard (simulate (0,0) (0,1) (a ++ [r] ++ bs) == (x,y))
            pure (i,r)
    putStrLn $ case solulu of
        ((i,r):_) -> show (i+1) ++ " " ++ show r
        []        -> "bruh" --error "bruh"

simulate :: (Int,Int) -> (Int,Int) -> [Instruction] -> (Int,Int)
simulate (x,y) _ []                 = (x,y)
simulate (x,y) (dx,dy) (Forward:xs) = simulate (x+dx,y+dy) (dx ,dy ) xs
simulate (x,y) (dx,dy) (Left   :xs) = simulate (x,y)       (-dy,dx ) xs
simulate (x,y) (dx,dy) (Right  :xs) = simulate (x,y)       (dy ,-dx) xs
