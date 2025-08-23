import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (delete)
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S
import           Data.Tuple            (swap)

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines
    map C.unpack rest
        & splitAt (readInt n)
        & swap
        & S.fromList *** combinations
        & uncurry (count 0)
        & print

count :: Int -> S.Set String -> [String] -> Int
count ret _ [] = ret
count ret dict (x:xs) | S.member x dict = count (1+ret) (S.delete x dict) xs
                      | otherwise       = count ret dict xs

combinations :: [String] -> [String]
combinations [] = [""]
combinations xss = do
    xs <- xss
    x <- xs
    map (x:) (combinations (delete xs xss))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
