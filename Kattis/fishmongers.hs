import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:fish:monger <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    print (fishmonger (sortOn (last >>> negate) monger) (sortOn negate fish))

fishmonger :: [[Int]] -> [Int] -> Int
fishmonger [] _ = 0
fishmonger _ [] = 0
fishmonger ([x,p]:xs) fish = splitAt x fish
    & (map (p*) >>> sum) *** fishmonger xs
    & uncurry (+)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
