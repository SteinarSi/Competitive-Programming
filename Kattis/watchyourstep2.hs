import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [sx,sy,tx,ty,ax,ay,bx,by] <- getLine <&> (words >>> map read)

    let row :: Int -> [Int] -> Int
        row y prev
            | y > ty    = last prev
            | otherwise = let next = zipWith3 (\p n x -> bool (p+n) 0 ((x,y)==(ax,ay)||(x,y)==(bx,by))) prev (0:next) [sx..]
                          in  row (y+1) next

    print (row sy (1 : replicate (tx - sx) 0))
