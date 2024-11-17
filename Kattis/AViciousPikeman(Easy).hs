import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (foldl', sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,t,a,b,c,x] <- C.getContents <&> (C.words >>> map readInt)

    let solved = problems a b c x
            & take n
            & sort
            & scanl (+) 0
            & takeWhile (<=t)

        total = length solved - 1
        penalty = foldl' addMod 0 solved

    putStrLn (show total <> " " <> show penalty)

problems :: Int -> Int -> Int -> Int -> [Int]
problems a b c = iterate ((*a) >>> (+b) >>> (`mod`c) >>> succ)

m :: Int
m = 1000000007

addMod :: Int -> Int -> Int
addMod a b = (a + b) `mod` m

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
