import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b)))
        >>> zip [1..]
        >>> mapM_ (\(i, (a,b)) -> C.putStrLn (C.pack ("Case " <> show i <> ": " <> show (solulu a b))))
    )

solulu :: Int -> Int -> Int
solulu 0 0 = 0
solulu 0 m = m
    & iterate (subtract earth >>> (`mod` mars))
    & elemIndex 0
    & fromJust
    & (earth*)
    & (lcm earth mars-)
solulu e m = earth - e + solulu 0 (m + earth - e)

earth :: Int
earth = 365

mars :: Int
mars = 687

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

