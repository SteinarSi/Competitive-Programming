import           Control.Arrow         (second, (>>>))
import           Data.Array.Unboxed    (UArray, assocs, bounds, inRange,
                                        listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    whn:xy:xss <- C.getContents <&> C.lines

    let
        [w,h,n] = map readInt (C.words whn)
        [x,y] = map readInt (C.words xy)
        grid = listArray ((0,0),(h-1,w-1)) (concatMap C.unpack xss)

    assocs grid
        & filter (snd >>> (`elem` "-|"))
        & map (fst >>> (,True))
        & M.fromList
        & simulate grid (y,x)
        & take n
        & map (\(y,x) -> show x <> " " <> show y)
        & unlines
        & putStr

simulate :: UArray (Int,Int) Char -> (Int,Int) -> M.Map (Int, Int) Bool -> [(Int,Int)]
simulate grid start saws = marble saws start
    & second (simulate grid start)
    & uncurry (:)
  where
    marble :: M.Map (Int, Int) Bool -> (Int,Int) -> ((Int,Int),M.Map (Int, Int) Bool)
    marble ss (y,x)
        | not (inRange (bounds grid) (y,x)) = ((y,x),ss)
        | otherwise = case grid ! (y,x) of
            '.'     -> ((y,x),ss)
            '<'     -> marble ss (y,x-1)
            '>'     -> marble ss (y,x+1)
            '^'     -> marble ss (y-1,x)
            'v'     -> marble ss (y+1,x)
            '-' | ss M.! (y,x) -> fmap (M.insert (y,x) False) (marble ss (y,x-1))
                | otherwise    -> fmap (M.insert (y,x) True ) (marble ss (y,x+1))
            '|' | ss M.! (y,x) -> fmap (M.insert (y,x) False) (marble ss (y-1,x))
                | otherwise    -> fmap (M.insert (y,x) True ) (marble ss (y+1,x))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
