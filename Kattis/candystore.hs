{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    names  <- replicateM n C.getLine <&> foldr (\x -> M.insertWith (++) (initials x) [x]) M.empty
    C.getContents >>= (
                C.lines
            >>> map (\ii -> case M.lookup ii names of
                    Nothing  -> "nobody"
                    Just [x] -> x
                    _        -> "ambiguous"
                )
            >>> mapM_ C.putStrLn
        )

initials :: C.ByteString -> C.ByteString
initials = C.words >>> map C.head >>> C.pack

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
