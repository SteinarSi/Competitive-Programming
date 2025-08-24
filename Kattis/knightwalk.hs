import           Control.Arrow      ((>>>))
import           Control.Monad      (filterM)
import           Control.Monad.ST   (ST, runST)
import           Data.Array.ST      (STUArray, newArray, readArray, runSTUArray,
                                     writeArray)
import           Data.Array.Unboxed (UArray, (!))
import           Data.Char          (chr, digitToInt, ord)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Data.Ix            (inRange)
import           Data.List          (intercalate, sort)
import           Data.Sequence      (Seq (..))
import qualified Data.Sequence      as Seq

main :: IO ()
main = do
    [s,t] <- getContents <&> (lines >>> map (\[x,y] -> (ord x - ord 'a' + 1, digitToInt y)))

    let
        dist :: UArray (Int,Int) Int
        dist = runSTUArray $ do
            ret <- newArray rng (-1)
            writeArray ret t 0
            bfs ret (Seq.singleton (0,t))
            pure ret

        bfs :: STUArray s (Int,Int) Int -> Seq (Int,(Int,Int)) -> ST s ()
        bfs ret Empty          = pure ()
        bfs ret ((i,u) :<| xs) = neighbours u
            & filterM (\v -> readArray ret v >>= \r -> if r == -1 then writeArray ret v (i+1) >> pure True else pure False)
            >>= (map (i+1,) >>> Seq.fromList >>> (xs<>) >>> bfs ret)

        path :: (Int,Int) -> [[(Int,Int)]]
        path u | u == t    = [[t]]
               | otherwise = neighbours u
                    & filter ((dist!) >>> (==dist!u-1))
                    & concatMap (path >>> map (u:))

    path s
        & map (map (\(c,r) -> chr (c + ord 'a' - 1) : show r) >>> intercalate " -> ")
        & sort
        & unlines
        & putStr

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) = filter (inRange rng) [(x+1,y+2),(x+2,y+1),(x+2,y-1),(x+1,y-2),(x-1,y-2),(x-2,y-1),(x-2,y+1),(x-1,y+2)]

rng :: ((Int,Int),(Int,Int))
rng = ((1,1),(8,8))
