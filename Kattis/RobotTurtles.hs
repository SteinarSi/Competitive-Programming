{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow      ((>>>))
import           Control.Monad.ST   (ST, runST)
import           Data.Array.ST      (STUArray, newArray, readArray, writeArray)
import           Data.Array.Unboxed (UArray, bounds, listArray, (!))
import           Data.Bool          (bool)
import           Data.Char          (isControl)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Data.Ix            (inRange)
import           Data.Maybe         (catMaybes)
import qualified Data.Set           as S

main :: IO ()
main = do
    grid <- getContents <&> (filter (isControl >>> not) >>> listArray ((1,1),(8,8)))

    putStrLn $ runST $ do
        memo <- newArray ((0,0),(9,9)) False
        (map (0,) [0..9] <> map (9,) [0..9] <> map (,0) [1..8] <> map (,9) [1..8])
            & mapM_ (flip (writeArray memo) True)
        solve grid memo (S.singleton (0, (8,1),(0,1),""))

solve :: forall s. UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> S.Set (Int, (Int,Int), (Int,Int), String) -> ST s String
solve grid memo xs
    | S.null xs         = pure "no solution"
    | grid ! pos == 'D' = pure (reverse moves)
    | otherwise         = do
        s <- readArray memo pos
        if s
            then solve grid memo ys
            else do
                writeArray memo pos True
                zs <- catMaybes <$> sequence [
                    turn ((-dx, dy), 'L'),
                    turn ((dx, -dy), 'R'),
                    do
                        let p = pos +++ dir
                        s <- readArray memo p
                        pure $ if | s               -> Nothing
                                  | grid ! p == 'I' -> Just (cost+2,p,dir,'F':'X':moves)
                                  | grid ! p == 'C' -> Nothing
                                  | otherwise       -> Just (cost+1,p,dir,'F':moves)
                    ]
                solve grid memo (ys <> S.fromList zs)
  where
    Just ((cost,pos@(y,x),dir@(dy,dx),moves), ys) = S.minView xs

    turn :: ((Int,Int),Char) -> ST s (Maybe (Int, (Int,Int),(Int,Int),String))
    turn (d,t) = do
        let p = pos +++ d
        s <- readArray memo p
        pure $ if | s               -> Nothing
                  | grid ! p == 'I' -> Just (cost+3,p,d,'F':'X':t:moves)
                  | grid ! p == 'C' -> Nothing
                  | otherwise       -> Just (cost+3,p,d,'F':t:moves)

(+++) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+++) (a,b) (p,q) = (a+p,b+q)
