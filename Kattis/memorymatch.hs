{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Either           (partitionEithers)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, isJust, isNothing)

main :: IO ()
main = do
    [n]:_:xs <- C.getContents <&> (C.lines >>> map C.words)
    let (done,partial) = partitionEithers (play M.empty xs)
        two  = length (filter isNothing partial)
        one  = length (filter isJust partial)
        none = readInt n `div` 2 - length done - two - one
    print if
        | none == 0             -> two + one
        | one == 0 && none == 1 -> two + 1
        | otherwise             -> two

play :: M.Map C.ByteString (Either () (Maybe C.ByteString)) -> [[C.ByteString]] -> [Either () (Maybe C.ByteString)]
play memo []     = M.elems memo
play memo ([c1,c2,p1,p2]:xs)
    | p1 == p2  = play (M.insert p1 (Left ()) memo) xs
    | otherwise = play (insert c1 p1 (insert c2 p2 memo)) xs
  where
    insert :: C.ByteString -> C.ByteString -> M.Map C.ByteString (Either () (Maybe C.ByteString)) -> M.Map C.ByteString (Either () (Maybe C.ByteString))
    insert c p m = case M.lookup p m of
        Nothing -> M.insert p (Right (Just c)) m
        Just (Left _) -> error "bruh!"
        Just (Right Nothing) -> m
        Just (Right (Just q))
            | q == c -> m
            | otherwise -> M.insert p (Right Nothing) m

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
