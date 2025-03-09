{-# LANGUAGE LambdaCase #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,ys] <- C.getContents <&> C.lines
    let (h,v,r) = C.foldl' (\(h,v,r) -> \case 'h' -> (h+1,v,r); 'v' -> (h,v+1,r); 'r' -> (h,v,r+1)) (0,0,0) ys

        horizontal | odd h     = C.reverse >>> C.map (\case 'b' -> 'd'; 'd' -> 'b'; 'p' -> 'q'; 'q' -> 'p')
                   | otherwise = id
        vertical   | odd v     =               C.map (\case 'b' -> 'p'; 'p' -> 'b'; 'q' -> 'd'; 'd' -> 'q')
                   | otherwise = id
        rotation   | odd r     = C.reverse >>> C.map (\case 'b' -> 'q'; 'q' -> 'b'; 'p' -> 'd'; 'd' -> 'p')
                   | otherwise = id

    xs
        & horizontal
        & vertical
        & rotation
        & C.putStrLn
