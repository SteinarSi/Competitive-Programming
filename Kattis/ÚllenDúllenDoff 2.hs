import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    x:xs <- C.getContents <&> C.lines
    forM_ (insert (12 `mod` n) x xs) C.putStrLn

insert :: Int -> a -> [a] -> [a]
insert 0 a xs     = a : xs
insert i a (x:xs) = x : insert (i-1) a xs
insert _ _ []     = error "bruh"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
