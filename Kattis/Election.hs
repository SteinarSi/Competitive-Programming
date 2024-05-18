{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    (party, votes) <- C.getContents <&> (
            C.lines
        >>> splitAt (2*n)
        >>> reg *** tail
        )
    count party votes
        & elect party
        & C.putStrLn

elect :: M.Map C.ByteString C.ByteString -> M.Map C.ByteString Int -> C.ByteString
elect party votes = case sortOn (snd >>> negate) (M.assocs votes) of
    []                           -> "tie"
    ((x,v1):(y,v2):_) | v1 == v2 -> "tie"
    ((x,_):_)                    -> M.lookup x party & fromJust

count :: M.Map C.ByteString C.ByteString -> [C.ByteString] -> M.Map C.ByteString Int
count _ [] = M.empty
count party (x:xs) | M.member x party = M.insertWith (+) x 1 (count party xs)
                   | otherwise = count party xs

reg :: [C.ByteString] -> M.Map C.ByteString C.ByteString
reg []       = M.empty
reg (x:y:xs) = M.insert x y (reg xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
