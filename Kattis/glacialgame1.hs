import           Control.Arrow      ((&&&), (>>>))
import           Data.Array.Unboxed (UArray, listArray, range, (!), (!?))
import           Data.Char          (isSpace)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Data.Maybe         (mapMaybe)

main :: IO ()
main = do
    board <- getContents <&> (filter (isSpace >>> not) >>> listArray rng)
    let (y,x) = range rng
            & filter ((board!) >>> (=='.'))
            & map ((score board >>> negate) &&& id)
            & minimum
            & snd
    putStrLn (show y <> " " <> show x)

score :: UArray (Int,Int) Char -> (Int,Int) -> Int
score board u = sum (mapMaybe (\dir -> flips dir (dir+++u)) [(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1)])
  where
    flips :: (Int,Int) -> (Int,Int) -> Maybe Int
    flips dir v = case board !? v of
        Nothing  -> Nothing
        Just 'W' -> succ <$> flips dir (dir+++v)
        Just 'B' -> Just 0
        Just '.' -> Nothing

rng :: ((Int,Int), (Int,Int))
rng = ((0,0),(7,7))

(+++) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+++) (a,b) (x,y) = (a+x,b+y)

