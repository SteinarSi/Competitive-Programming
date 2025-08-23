import           Control.Arrow         ((>>>))
import           Data.Array.ST         (STUArray, newArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (Ix, UArray, assocs, bounds, inRange,
                                        listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, isSpace)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    Just (n, rest) <- C.getContents <&> C.readInt

    rest
        & C.filter (isSpace >>> not)
        & C.unpack
        & listArray ((1,1),(n,n))
        & solve
        & bool 0 1
        & print

solve :: UArray (Int,Int) Char -> Bool
solve grid = all legal (assocs grid)
  where
    lit :: UArray (Int,Int) Bool
    lit = runSTUArray $ do
        light <- newArray (bounds grid) False
        assocs grid
            & filter (snd >>> (=='?'))
            & concatMap (fst >>> ray >>> (`concatMap` dirs))
            & mapM_ (flip (writeArray light) True)
        pure light
      where
        ray :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
        ray pos dir = iterate (dir+++) pos
            & drop 1
            & takeWhile (inRange (bounds grid))
            & takeWhile ((grid!) >>> (`elem` ".?"))
          where next = pos +++ dir

    legal :: ((Int,Int), Char) -> Bool
    legal (pos,c) = case c of
            '.' -> lit ! pos
            'X' -> True
            '?' -> not (lit ! pos)
            x   -> neighbours == digitToInt x
      where
        neighbours = map (pos+++) dirs
            & filter (inRange (bounds grid))
            & filter ((grid!) >>> (=='?'))
            & length

dirs :: [(Int,Int)]
dirs = [(1,0),(-1,0),(0,1),(0,-1)]

(+++) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+++) (a,b) (x,y) = (a+x,b+y)
