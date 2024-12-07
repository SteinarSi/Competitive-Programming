import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> foldr (overlap >>> (=<<)) (Just (minBound,maxBound))
        >>> maybe "bad news" (\(a,b) -> show (b-a+1) <> " " <> show a)
        >>> putStrLn
    )

overlap :: (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
overlap (a,b) (x,y) | ax <= by  = Just (ax,by)
                    | otherwise = Nothing
    where ax = max a x
          by = min b y

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
