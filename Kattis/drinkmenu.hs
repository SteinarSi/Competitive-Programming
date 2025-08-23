import           Control.Arrow         (first, (&&&), (>>>))
import           Data.Array            (listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,xs) <- C.getContents <&> (C.lines >>> (head >>> readInt) &&& tail)

    let (menu, orders) = splitAt n xs
            & first (listArray (1,n))

    serve M.empty [] orders
        & map (menu !)
        & C.unlines
        & C.putStr

serve :: M.Map C.ByteString Int -> [Int] -> [C.ByteString] -> [Int]
serve record ret [] = reverse ret
serve record ret (x:xs) = serve (M.insert x (order+1) record) (order:ret) xs
    where
        order = M.findWithDefault 1 x record

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
