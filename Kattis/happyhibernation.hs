import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:h:t:xs <- C.getContents <&> (C.words >>> map readInt)

    map (<=t) xs
        & hibernate h h 0
        & maybe "Too hot!" show
        & putStrLn

hibernate :: Int -> Int -> Int -> [Bool] -> Maybe Int
hibernate h 0 i _ = Just i
hibernate _ _ _ [] = Nothing
hibernate h r i (x:xs) | x         = hibernate h (r-1) i xs
                       | otherwise = hibernate h h (i+h-r+1) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
