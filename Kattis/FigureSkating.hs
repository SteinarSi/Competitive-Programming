import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (maximumBy)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (pred,final) <- C.getContents <&> (C.lines
        >>> (head >>> readInt) &&& tail
        >>> uncurry splitAt)
    let p = M.fromList (zip pred [1..])
    C.putStrLn $ if pred == final
        then C.pack "suspicious"
        else zip [1..] final
            & maximumBy (compare `on` (\(i,x) -> (p M.! x - i, -i)))
            & snd

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
