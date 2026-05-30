import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (delete, find)

main :: IO ()
main = do
    ((f,l),xs) <- C.getContents <&> (C.lines >>> ((head >>> C.head &&& C.last) &&& drop 2))

    let survive = filter (C.head >>> (==l)) xs
        win = filter (\x -> C.head x == l && all (C.head >>> (/=C.last x)) (delete x xs)) survive

    C.putStrLn $ case (survive,win) of
        ([],[])  -> C.pack "?"
        (_,x:_)  -> x `C.snoc` '!'
        (x:_,[]) -> x
