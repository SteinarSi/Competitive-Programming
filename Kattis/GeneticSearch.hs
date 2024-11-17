import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.Function ((&))
import           Data.List (isPrefixOf, tails)
import qualified Data.Set as S

main :: IO ()
main = getContents >>= (
            lines
        >>> init
        >>> mapM_ (words
            >>> (\(a:b:_) -> solve a b)
            >>> putStrLn
        )
    )

solve :: String -> String -> String
solve s t = map show [t1, t2, t3] & unwords
    where
        t1 = tails t
            & filter (isPrefixOf s)
            & length
        t2 = length $ do
                t' <- tails t
                s' <- allType2s s & S.toList
                guard (s' `isPrefixOf` t') 
                pure ()
        t3 = length $ do
                t' <- tails t
                s' <- allType3s s & S.toList
                guard (s' `isPrefixOf` t')
                pure ()

allType2s :: String -> S.Set String
allType2s "" = S.empty
allType2s [x] = S.fromList [[]]
allType2s (x:xs) = allType2s xs
        & S.map (x:)
        & S.insert xs

allType3s :: String -> S.Set String
allType3s "" = S.fromList (map pure "GACT")
allType3s (x:xs) = map (:x:xs) "GACT"
        & S.fromList
        & S.union (S.map (x:) (allType3s xs))
