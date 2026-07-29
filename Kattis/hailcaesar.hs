import           Control.Arrow ((>>>))
import           Data.Char     (chr, ord)
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    xs:xss <- getContents <&> lines
    let s = 33 - ord (last xs)
    map (shift s) (xs:xss)
        & unlines
        & putStr

shift :: Int -> String -> String
shift s = map (ord >>> (+s) >>> subtract 32 >>> (`mod`(127-32)) >>> (+32) >>> chr)
