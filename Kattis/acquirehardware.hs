import           Control.Arrow         ((>>>))
import           Data.Array            (Array, range)
import           Data.Array.Base       (UArray, bounds, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    h:w:xs <- C.getContents <&> C.words

    print $ solve $ listArray ((1,1),(readInt h, readInt w)) (concatMap C.unpack xs)

solve :: UArray (Int,Int) Char -> Int
solve grid = dp ! (1,1)
  where
    (h,w) = snd (bounds grid)

    dp :: Array (Int,Int) Int
    dp = listArray (bounds grid) (map f (range (bounds grid)))

    f :: (Int,Int) -> Int
    f (y,x) = bool 0 1 (grid ! (y,x) == 'I') + max (bool 0 (dp ! (y+1,x)) (y < h)) (bool 0 (dp ! (y,x+1)) (x < w))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
