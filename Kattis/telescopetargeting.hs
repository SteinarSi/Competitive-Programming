import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, assocs, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    wr':hr':rest <- C.getContents <&> C.words

    let wr = readInt wr'
        hr = readInt hr'
        wi = readInt wi'
        hi = readInt hi'
        (rs,wi':hi':is) = splitAt hr rest

        ref :: UArray (Int,Int) Char
        ref = listArray ((1,1),(hr,wr)) (concatMap C.unpack rs)

        img :: UArray (Int,Int) Char
        img = listArray ((1,1),(hi,wi)) (concatMap C.unpack is)

        Just (y,x) = [(dy,dx) | dy <- [0..hi-hr], dx <- [0..wi-wr]]
            & find (\(dy,dx) -> all (\((y,x),r) -> r == img ! (y+dy,x+dx)) (assocs ref))

    putStrLn (show (x - (wi-wr) `div` 2) <> " " <> show (y - (hi-hr) `div` 2))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
