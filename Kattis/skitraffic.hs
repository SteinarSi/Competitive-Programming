import           Control.Arrow ((***), (>>>))
import           Data.Bool     (bool)
import           Data.Char     (isDigit)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    [_,exp,day,w,s,h] <- getContents <&> lines

    span isDigit exp
        & (read >>> (*(60::Int))) *** (drop 1 >>> read)
        & uncurry (+)
        & (* bool 1 2 (day == "sat" || day == "sun"))
        & (* bool 1 2 (w == "1"))
        & (* bool 1 3 (s == "1"))
        & (* bool 1 3 (h == "1"))
        & (`quotRem` 60)
        & uncurry (printf "%d:%.2d\n")
