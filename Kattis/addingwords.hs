{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         ((>=>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as IM
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, fromMaybe)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> compute IM.empty M.empty
        >>> C.unlines
        >>> C.putStr
    )

compute :: IM.IntMap C.ByteString -> M.Map C.ByteString Int -> [C.ByteString] -> [C.ByteString]
compute numbers words [] = []
compute numbers words (x:xs) = case C.words x of
    ["clear"]     -> compute IM.empty M.empty xs
    ["def",var,n'] -> let n = readInt n'
                          m = M.findWithDefault (-1001) var words
                      in  compute (IM.insert n var (IM.delete m numbers)) (M.insert var n words) xs
    ("calc":c)    -> calc c : compute numbers words xs
  where
    calc :: [C.ByteString] -> C.ByteString
    calc = (extract >=> (`IM.lookup` numbers))
        >>> fromMaybe "unknown"
        >>> ((C.drop 5 x <> " ") <>)

    extract :: [C.ByteString] -> Maybe Int
    extract []  = Just 0
    extract [_] = Just 0
    extract ("+":var:vs) = liftA2 (+) (M.lookup var words) (extract vs)
    extract ("-":var:vs) = do
        v <- M.lookup var words
        rs <- extract vs
        pure (rs - v)
    extract (var:vs) = liftA2 (+) (M.lookup var words) (extract vs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
