import           Control.Arrow            ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Functor             ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe               (fromJust)

main :: IO ()
main = do
    (n,xs) <- C.getContents <&> (C.words >>> map readInt >>> head &&& (drop 1 >>> S.fromList))

    let alice = S.toAscList xs
        bob = filter (`S.notMember` xs) [1..2*n]

        best  = play alice bob
        worst = n - play bob alice

    putStrLn (show worst <> " " <> show best)

play :: [Int] -> [Int] -> Int
play [] _ = 0
play _ [] = error "bruh"
play (x:xs) (y:ys) | x > y     = 1 + play xs ys
                   | otherwise = play xs (y:ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
