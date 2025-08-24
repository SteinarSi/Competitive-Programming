import           Control.Arrow ((>>>))
import           Data.Char     (ord)
import           Data.Functor  ((<&>))
import qualified Data.IntSet   as S

main :: IO ()
main = do
    xs <- getLine <&> (map ord >>> S.fromList)
    let missed = filter (ord >>> (`S.notMember` xs)) ['A'..'Z']
    putStrLn $ if null missed
        then "Alphabet Soup!"
        else missed
