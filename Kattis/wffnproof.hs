import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isLower, isUpper)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.unpack >>> parse >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

parse :: String -> (Int, String, String)
parse xs = (
        length (filter (=='N') xs),
        filter (\x -> (x/='N') && isUpper x) xs,
        filter isLower xs
    )

solve :: (Int, String, String) -> C.ByteString
solve (_ , _  , ""  ) = C.pack "no WFF possible"
solve (ns, ops, vars) = C.replicate ns 'N' <> C.pack (solulu ops vars)
    where
        solulu :: String -> String -> String
        solulu _ ""            = error "bruh"
        solulu _ [x]           = [x]
        solulu [] (x:_)        = [x]
        solulu (o:os) (x:y:xs) = o : x : solulu os (y:xs)
