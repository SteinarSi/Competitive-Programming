import           Control.Applicative ((<|>))
import           Control.Arrow       ((>>>))
import           Data.Array          (Array, (!))
import           Data.Array.ST       (newArray, runSTArray, writeArray)
import           Data.Maybe          (fromMaybe)

main :: IO ()
main = interact (read >>> solve >>> fromMaybe "impossible")

solve :: Int -> Maybe String
solve n = foldr (<|>) Nothing (single ! n : map split [(n+1)`div`2 .. n-4])
    where
        split :: Int -> Maybe String
        split a = do
            x <- single ! a
            y <- single ! (n-a)
            Just (x <> " " <> y)

single :: Array Int (Maybe String)
single = runSTArray $ do
    arr <- newArray (4,1000) Nothing
    sequence_ $ do
        a <- [2..32]
        b <- takeWhile ((a*) >>> (<=1000)) [a..]
        pure (writeArray arr (a*b) (Just (show a <> "x" <> show b)))
    pure arr
