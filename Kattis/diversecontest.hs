import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ((n,k),xs) <- C.getContents <&> (
                C.lines
            >>> (head >>> C.words >>> map readInt >>> head &&& last)
                    &&&
                (tail >>> map (C.words >>> tail)))
    print $ diversify (k `div` 2) k M.empty xs

diversify :: Int -> Int -> M.Map C.ByteString Int -> [[C.ByteString]] -> Int
diversify limit 0 counts _       = 1
diversify limit k counts []      = 0
diversify limit k counts (xs:xss) | allowed   = diversify limit k counts xss + diversify limit (k-1) pick xss
                                  | otherwise = diversify limit k counts xss
    where
        pick = foldr (flip (M.insertWith (+)) 1) counts xs
        allowed = all (flip (M.findWithDefault 0) counts >>> (<limit)) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
