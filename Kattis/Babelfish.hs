{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Data.Bifunctor        (Bifunctor (bimap))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Map              (Map, findWithDefault, fromList, insert)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> break (""==)
        >>> bimap (map (C.words >>> (\(a:b:_) -> (b,a))) >>> fromList) tail
        >>> (\(dic, sentence) -> map (flip (findWithDefault "eh") dic) sentence)
        >>> mapM_ C.putStrLn
    )
