import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (filterM, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, bounds, indices,
                                        listArray, newArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (STArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> split
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: UArray (Int,Int) Char -> Int
solve grid = indices grid
    & find ((grid!) >>> (=='S'))
    & fromJust
    & pure
    & assimilate
  where
    assimilate :: [(Int,Int)] -> Int
    assimilate src = let p = runST $ do
                                seen <- newArray (bounds grid) False
                                mapM_ (flip (writeArray seen) True) src
                                bfs seen 1 src
                     in  case p of
                            Nothing    -> 0
                            Just (s,u) -> s + assimilate (u:src)
      where
        bfs :: STUArray s (Int,Int) Bool -> Int -> [(Int,Int)] -> ST s (Maybe (Int,(Int,Int)))
        bfs seen steps [] = pure Nothing
        bfs seen steps xs = do
            ys <- mapM (\u@(y,x) -> [(y+1,x),(y-1,x),(y,x+1),(y,x-1)]
                    & filter ((grid!) >>> (/='#'))
                    & filterM (\v -> do
                        d <- readArray seen v <&> not
                        when d (writeArray seen v True)
                        pure d
                    )
                ) xs <&> concat
            case find ((grid!) >>> (=='A')) ys of
                Just  u -> pure (Just (steps,u))
                Nothing -> bfs seen (steps+1) ys

split :: [C.ByteString] -> [UArray (Int,Int) Char]
split [] = []
split (wh:xs) = splitAt h xs
    & (C.concat >>> C.unpack >>> listArray ((1,1),(h,w)))
        ***
      split
    & uncurry (:)
  where [w,h] = map readInt (C.words wh)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
