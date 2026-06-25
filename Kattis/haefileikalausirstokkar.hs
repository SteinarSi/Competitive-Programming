import           Control.Arrow         (second, (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines
    let (garbage,decks) = splitAt (readInt n) rest
            & S.fromList *** (drop 1 >>> chunksOf 6)
    decks
        & map (any (`S.member` garbage) >>> bool "Fínn Stokkur" "Hæfileikalaust Drasl")
        & unlines
        & putStr

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
