import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.unpack >>> take 80 >>> everyOther)
        >>> unlines
        >>> putStr
    )

everyOther :: String -> String
everyOther []       = []
everyOther [_]      = []
everyOther (x:_:xs) = x : everyOther xs
