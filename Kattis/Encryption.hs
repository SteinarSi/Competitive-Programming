import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,ys,zs] <- C.getContents <&> (C.lines >>> map C.unpack)
    putStrLn (encrypt xs ys zs)

encrypt :: String -> String -> String -> String
encrypt [] ys _  = ys
encrypt xs [] _  = xs
encrypt xs ys [] = xs <> ys
encrypt (x:xs) (y:ys) (z:zs) | x == y && y == z = z : encrypt xs     ys     zs
                             | x == z           = y : encrypt (x:xs) ys     (z:zs)
                             | otherwise        = x : encrypt xs     (y:ys) (z:zs)
