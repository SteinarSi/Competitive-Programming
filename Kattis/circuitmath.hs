import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    vals <- C.getLine <&> (C.words
            >>> map (C.head >>> (=='T'))
            >>> zip ['A'..]
            >>> M.fromList
        )
    expr <- C.getLine <&> (C.words >>> map C.head)

    eval vals [] expr
        & show
        & take 1
        & putStrLn

eval :: M.Map Char Bool -> [Bool] -> [Char] -> Bool
eval vals [x] []            = x
eval vals (x:y:xs) ('+':ys) = eval vals ((x || y) : xs) ys
eval vals (x:y:xs) ('*':ys) = eval vals ((x && y) : xs) ys
eval vals (x:xs) ('-':ys)   = eval vals (not x : xs) ys
eval vals xs (y:ys)         = eval vals (fromJust (M.lookup y vals) : xs) ys

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
