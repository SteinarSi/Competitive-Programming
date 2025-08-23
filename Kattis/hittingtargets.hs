{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

type Pos = (Int,Int)
data Shape = Circle Pos Int | Rectangle Pos Pos

main :: IO ()
main = do
    m <- C.getLine <&> readInt
    targets <- replicateM m C.getLine <&> map (C.words >>> parseShape)
    C.getContents >>= (
                C.lines
            >>> tail
            >>> mapM_ (
                    C.words
                >>> map readInt
                >>> (\[a,b] -> filter (overlap (a,b)) targets)
                >>> length
                >>> print
            )
        )

overlap :: Pos -> Shape -> Bool
overlap (x,y) (Circle (cx,cy) r) = abs (cx-x)^2 + abs (cy-y)^2 <= r*r
overlap (x,y) (Rectangle (rx1,ry1) (rx2,ry2)) = and [x >= rx1, x <= rx2, y >= ry1, y <= ry2]

parseShape :: [C.ByteString] -> Shape
parseShape ("rectangle":xs) = Rectangle (x1,y1) (x2,y2)
    where [x1,y1,x2,y2] = map readInt xs
parseShape ("circle":xs) = Circle (x,y) r
    where [x,y,r] = map readInt xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
