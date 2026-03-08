import           Control.Arrow      ((>>>))
import           Control.Monad.ST   (ST, runST)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import qualified Data.IntMap.Strict as M
import           Data.List          (delete, sort)
import           Data.STRef.Strict  (STRef, modifySTRef, newSTRef, readSTRef)
import           Text.Printf        (printf)

main :: IO ()
main = do
    [r,g,b,y,s] <- getLine <&> (words >>> map read)
    printf "%.5f\n" $ runST $ do
        memo <- newSTRef M.empty
        play memo ([r,g,b,y], s)

play :: STRef s (M.IntMap Double) -> ([Int],Int) -> ST s Double
play _ (_, 0) = pure 0
play _ ([0,0,0,0], _) = pure 1
play memo (xs@[r,g,b,y],s) = do
    let h = hash (xs, s)
    done <- readSTRef memo
    case M.lookup h done of
        Just  p -> pure p
        Nothing -> do
            let possibilities = [[r-1,g,b,y], [r,g-1,b,y], [r,g,b-1,y], [r,g,b,y-1]]
                    & filter (all (>=0))
                    & map (,s)
                    & ((xs, s-1) :)
                    & ((basket xs, s) :)
            p <- mapM (play memo) possibilities <&> (map (/fromIntegral (length possibilities)) >>> sum)
            modifySTRef memo (M.insert h p)
            pure p

basket :: [Int] -> [Int]
basket xs = m-1 : delete m xs
  where
    m = maximum xs

hash :: ([Int],Int) -> Int
hash (xs,s) = x1 + 4 * x2 + 16 * x3 + 64 * x4 + 1024 * s
  where
    [x1,x2,x3,x4] = sort xs
