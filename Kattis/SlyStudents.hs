import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = C.getLine >>= (C.words >>> mapM_ (solve >>> C.putStrLn))

solve :: C.ByteString -> C.ByteString
solve word = C.unlines [
        C.pack (show d),
        C.unwords (map encode vals)
    ] & C.init
    where
        vals = map ord (C.unpack word)
        d = foldr1 gcd vals

        encode v = v `div` d
            & ternary
            & reverse
            & concatMap show
            & C.pack

        ternary 0 = []
        ternary v = v `mod` 3 : ternary (v `div` 3)
