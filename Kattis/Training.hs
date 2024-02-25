import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,s):xs <- C.getContents <&> (
            C.lines
        >>> map (C.words
            >>> map (C.readInt >>> fromJust >>> fst)
            >>> (\[l,u] -> (l,u))
            )
        )
    print (train s xs)

train :: Int -> [(Int, Int)] -> Int
train s [] = s
train s ((l,u):xs) | l <= s && s <= u = train (s+1) xs
                   | otherwise = train s xs
