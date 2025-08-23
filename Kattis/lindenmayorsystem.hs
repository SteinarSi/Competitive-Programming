import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (find, isPrefixOf)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    nm:xs <- C.getContents <&> C.lines
    let [n, m] = map readInt (C.words nm)
        rules  = map (C.words >>> (\xs -> (C.unpack (head xs), C.unpack (last xs)))) (init xs)
        s      = C.unpack (last xs)
    putStrLn (iterate (replace rules) s !! m)

replace :: [(String, String)] -> String -> String
replace _ [] = []
replace rules (x:xs) = case find (fst >>> (`isPrefixOf` (x:xs))) rules of
    Nothing    -> x : replace rules xs
    Just (a,b) -> b <> replace rules (drop (length a) (x:xs))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
