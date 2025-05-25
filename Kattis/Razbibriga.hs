import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (forM_)
import           Data.Array.Base       (UArray, newArray, readArray, writeArray,
                                        (!))
import           Data.Array.ST         (runSTUArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.head &&& C.last)
        >>> count
        >>> solve
        >>> print
    )

solve :: UArray (Char,Char) Int -> Int
solve cnt = sum $ do
    nw <- alphabet
    ne <- alphabet
    sw <- alphabet
    se <- alphabet
    let (top,bot,left,right) = ((nw,ne),(sw,se),(nw,sw),(ne,se))
    pure $ product [
        cnt ! top,
        cnt ! bot   - occurences bot   [top],
        cnt ! left  - occurences left  [top,bot],
        cnt ! right - occurences right [top,bot,left]
        ]

occurences :: Eq a => a -> [a] -> Int
occurences a [] = 0
occurences a (x:xs) | a == x    = 1 + occurences a xs
                    | otherwise =     occurences a xs

count :: [(Char,Char)] -> UArray (Char,Char) Int
count xs = runSTUArray $ do
    arr <- newArray (('A','A'),('Z','Z')) 0
    forM_ xs $ \ab -> readArray arr ab >>= (succ >>> writeArray arr ab)
    pure arr

alphabet :: [Char]
alphabet = ['A'..'Z']
