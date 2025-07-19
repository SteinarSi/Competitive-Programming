{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative   ((<|>))
import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

main :: IO ()
main = do
    ([s,t],graph) <- C.getContents <&> (
                C.lines
            >>> map C.words
            >>> last
                &&&
                (init
                    >>> concatMap (\(u:vs) -> map (S.singleton >>> (u,)) vs <> map (,S.singleton u) vs)
                    >>> M.fromListWith (<>)
                    >>> fmap S.toList)
        )

    let search :: C.ByteString -> C.ByteString -> Maybe [C.ByteString]
        search prev curr | curr == t = Just [curr]
                         | otherwise = M.findWithDefault [] curr graph
            & filter (/=prev)
            & foldr (search curr >>> (<|>)) Nothing
            <&> (curr:)

    search s s
        & maybe "no route found" C.unwords
        & C.putStrLn
