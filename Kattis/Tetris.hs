import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (nub, tails)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    c:p:xs <- C.getContents <&> (C.words >>> map readInt)

    print $ length [() | piece <- pieces !! (p-1), land <- tails xs, length land >= length piece, fit piece land]


fit :: [Int] -> [Int] -> Bool
fit piece = zipWith (-) piece
        >>> nub
        >>> length
        >>> (==1)

pieces :: [[[Int]]]
pieces = [
        [[0], [0,0,0,0]],
        [[0,0]],
        [[0,0,1], [1,0]],
        [[1,0,0], [0,1]],
        [[0,0,0], [0,1], [1,0,1], [1,0]],
        [[0,0,0], [0,0], [0,1,1], [2,0]],
        [[0,0,0], [0,2], [1,1,0], [0,0]]
    ]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
