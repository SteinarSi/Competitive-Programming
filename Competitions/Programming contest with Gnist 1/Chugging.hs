import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n, ta, da, tb, db] <- C.getContents <&> (C.words >>> map readInt)

    let alice = sum $ scanl (+) ta (replicate (n-1) da)
        bob   = sum $ scanl (+) tb (replicate (n-1) db)
    putStrLn $ if alice == bob
        then "="
        else if alice < bob
            then "Alice"
            else "Bob"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
