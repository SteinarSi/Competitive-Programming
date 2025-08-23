{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM, forM_, (>=>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Ix (inRange, range))
import           Data.Array.Base       (MArray (newArray), STUArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isSpace)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (iterate')
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [r,c] <- C.getLine <&> (C.words >>> map readInt)

    let rng = ((1,1),(r,c))
    (start, end, knights) <- C.getContents <&> (
                C.filter (isSpace >>> not)
            >>> C.unpack
            >>> zip (range rng)
            >>> parse undefined undefined []
        )

    putStrLn $ runST $ do
        attackers <- newArray rng 0
        searched  <- newArray rng False
        occupied  <- newArray rng False

        forM_ knights $ \k -> do
            writeArray occupied k True
            forM_ (jump rng k) (flip (modifyArray attackers) succ)
        writeArray searched start True

        solve rng attackers searched occupied start end

solve :: forall s. ((Int,Int),(Int,Int)) -> STUArray s (Int,Int) Int -> STUArray s (Int,Int) Bool -> STUArray s (Int,Int) Bool -> (Int,Int) -> (Int,Int) -> ST s String
solve rng attackers searched occupied start end = search [start]
  where
    search :: [(Int,Int)] -> ST s String
    search [] = pure "no"
    search (u:xs) = do
        ys <- mapM (attack >=> filterM unseen) (walks u)
        done <- readArray searched end
        if done
            then pure "yes"
            else search (concat ys <> xs)

    attack :: [(Int,Int)] -> ST s [(Int,Int)]
    attack [] = pure []
    attack (x:xs) = do
        s <- readArray searched x
        a <- readArray attackers x
        o <- readArray occupied x
        case (s,a,o) of
            (True,_,_) -> pure []
            (False,0,True) -> do
                writeArray occupied x False
                ys <- jump rng x
                    & filterM (\y -> do
                        a <- readArray attackers y
                        writeArray attackers y (a-1)
                        if a <= 1
                            then reachable y
                            else pure False
                        )
                ((x:ys)<>) <$> attack xs
            (False,0,False) -> (x:) <$> attack xs
            (False,_,True ) -> pure []
            (False,_,False) -> attack xs

    reachable :: (Int,Int) -> ST s Bool
    reachable = walks >>> anyM reach
      where
        reach :: [(Int,Int)] -> ST s Bool
        reach [] = pure False
        reach (x:xs) = do
            o <- readArray occupied x
            s <- readArray searched x
            if | o         -> pure False
               | s         -> pure True
               | otherwise -> reach xs

    unseen :: (Int,Int) -> ST s Bool
    unseen x = readArray searched x >>= bool (writeArray searched x True >> pure True) (pure False)

    walks :: (Int,Int) -> [[(Int,Int)]]
    walks u = [(1,0),(-1,0),(0,-1),(0,1)]
            & map (\(dy,dx) -> iterate' (\(y,x) -> (y+dy,x+dx)) u
                & drop 1
                & takeWhile (inRange rng))

jump :: ((Int,Int),(Int,Int)) -> (Int,Int) -> [(Int,Int)]
jump rng (y,x) = filter (inRange rng) [(y-2,x+1),(y-1,x+2),(y+1,x+2),(y+2,x+1),(y+2,x-1),(y+1,x-2),(y-1,x-2),(y-2,x-1)]

parse :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [((Int,Int),Char)] -> ((Int,Int), (Int,Int), [(Int,Int)])
parse start end knights [] = (start, end, knights)
parse start end knights ((x,c):xs) = case c of
        'K' -> parse start end (x:knights) xs
        'R' -> parse x     end knights     xs
        'T' -> parse start x   knights     xs
        _   -> parse start end knights     xs

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p []     = pure False
anyM p (x:xs) = p x >>= bool (anyM p xs) (pure True)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
