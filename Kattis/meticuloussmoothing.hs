import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.words >>> map readInt)
    print (smoothen False (0,[]) xs)

smoothen :: Bool -> (Int,[Int]) -> [Int] -> Int
smoothen False (r,_) []  = r
smoothen True (r,ret) [] = smoothen False (r,[]) ret
smoothen b (r,ret) [x]   = smoothen b (r,x:ret) []
smoothen b (r,ret) (x:y:xs)
    | x+1 < y   = smoothen True (r+y-(x+1),x:ret) (x+1:xs)
    | y+1 < x   = smoothen True (r+x-(y+1),y+1:ret) (y:xs)
    | otherwise = smoothen b (r,x:ret) (y:xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
