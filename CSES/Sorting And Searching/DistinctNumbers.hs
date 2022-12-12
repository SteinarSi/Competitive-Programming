{-# LANGUAGE Strict #-}

import qualified Data.IntSet as IS
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

main :: IO ()
main = C.interact $ C.pack . show . IS.size . IS.fromList . map (fst . fromJust . C.readInt) . tail . C.words