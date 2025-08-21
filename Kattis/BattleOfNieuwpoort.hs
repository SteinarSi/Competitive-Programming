import           Control.Arrow ((>>>))
import           Data.Char     (intToDigit)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (find, isSuffixOf)
import           Numeric       (showIntAtBase)
import           Text.Printf   (printf)

main :: IO ()
main = do
    y:_ <- getLine <&> (words >>> map read)
    [2..16::Int]
        & map (\b -> (b, showIntAtBase b intToDigit y ""))
        & find (snd >>> isSuffixOf "00")
        & maybe "impossible" (uncurry (printf "%d %s"))
        & putStrLn
