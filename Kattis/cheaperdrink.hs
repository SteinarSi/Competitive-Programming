{-# LANGUAGE LambdaCase #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (minimumBy, sortBy)

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> map parse
        >>> sortBy (\xs ys -> compare @Integer (read (xs <> ys)) (read (ys <> xs)))
        >>> concat
        >>> putStrLn
    )

parse :: String -> String
parse xs = case upsideDown xs of
    Nothing -> xs
    Just ys -> minimumBy (compare `on` read @Integer) [xs, ys]

upsideDown :: String -> Maybe String
upsideDown = mapM (\case
    '0' -> Just '0'
    '1' -> Just '1'
    '8' -> Just '8'
    '6' -> Just '9'
    '9' -> Just '6'
    _   -> Nothing)
    >>> fmap reverse

