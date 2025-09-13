import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getLine <&> (C.words >>> map readInteger)
    let
        p x = sum $ zipWith (*) (map (x^) [0..]) (reverse xs)
        c0 = p 0
        c1 = p 1 - c0
        c2 = p 2 - c0 - 2*c1
        c3 = p 3 - c0 - 3*c1 - 3*c2
        c4 = p 4 - c0 - 4*c1 - 6*c2 - 4*c3
        c5 = p 5 - c0 - 5*c1 - 10*c2 - 10*c3 - 5*c4
        c6 = p 6 - c0 - 6*c1 - 15*c2 - 20*c3 - 15*c4 - 6*c5

    [c0,c1,c2,c3,c4,c5,c6]
        & take (fromIntegral n + 1)
        & map show
        & unwords
        & putStrLn

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
