import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    xs <- C.getLine

    let rs = C.length (C.filter (=='r') xs)

    putStrLn $ if not (C.null xs) && C.head xs == 'b' && C.last xs `elem` "aeiouy" && rs >= 2 && rs == C.length xs - 2
        then "Jebb"
        else "Neibb"
