import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> parse
        >>> mapM_ (uncurry solve >>> C.putStrLn)
    )

solve :: Int -> C.ByteString -> C.ByteString
solve 0 = id
solve n = C.concatMap (\x -> bool (C.pack [x]) (C.pack ['\\',x]) (special x)) >>> solve (n-1)

special :: Char -> Bool
special x = '!' <= x && x <= '*' || '[' <= x && x <= ']'

parse :: [C.ByteString] -> [(Int,C.ByteString)]
parse []       = []
parse [_]      = error "bruh"
parse (x:y:xs) = (fst (fromJust (C.readInt x)), y) : parse xs
