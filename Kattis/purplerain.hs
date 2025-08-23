{-# LANGUAGE LambdaCase #-}

import           Control.Arrow ((&&&), (>>>))
import           Data.Functor  ((<&>))
import           Data.Ord      (comparing)

main :: IO ()
main = do
    xs <- getLine <&> map (\case 'R' -> 1; 'B' -> -1)

    let solve = zip [1..] >>> kadane Nothing Nothing
        Just ans = solve xs `max` solve (map negate xs)

    print ans

kadane :: Maybe Solution -> Maybe Solution -> [(Int,Int)] -> Maybe Solution
kadane b _ [] = b
kadane b curr ((i,x):xs) = kadane (max b next) next xs
    where
        (cs,cv) = maybe (i,0) (start &&& value) curr
        next | cv+x >= 0 = Just (Solution cs i (cv+x))
             | otherwise = Nothing

data Solution = Solution {
        start :: !Int,
        end   :: !Int,
        value :: !Int
    }
    deriving Eq

instance Show Solution where
    show Solution {start, end} = show start <> " " <> show end

instance Ord Solution where
    compare = comparing (value &&& (start >>> negate) &&& (end >>> negate))
