import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (x,y):_:monsters <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    putStrLn $ if any (\(mx,my) -> (x-mx)^2 + (y-my)^2 <= 8*8) monsters
        then "NO"
        else "YES"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
