{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [m, n] <- C.getLine <&> (C.words >>> map readInt)
    rest <- C.getContents <&> (C.lines >>> map C.words)

    let graph = take m rest
            & foldr (map (C.head >>> ord) >>> (\(a:b:_) -> M.insertWith (++) a [b])) M.empty
        memo = ['a'..'z']
            & map (ord >>> (\c -> (c, memoize graph (S.singleton c) c)))
            & M.fromList

    drop m rest
        & mapM_ ((\(a:b:_) -> match memo a b)
            >>> bool "no" "yes"
            >>> C.putStrLn
        )

memoize :: M.IntMap [Int] -> S.IntSet -> Int -> S.IntSet
memoize graph seen curr = S.foldr (memoize graph seen' >>> S.union) seen' next
    where next = M.findWithDefault [] curr graph
            & filter (`S.notMember` seen)
            & S.fromList
          seen' = S.union seen next

match :: M.IntMap S.IntSet -> C.ByteString -> C.ByteString -> Bool
match memo a b = C.length a == C.length b && and (C.zipWith matchLetter a b)
    where
        matchLetter :: Char -> Char -> Bool
        matchLetter from to = M.lookup (ord from) memo
            & fromJust
            & S.member (ord to)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
