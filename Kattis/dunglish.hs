{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (first, second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M

main :: IO ()
main = do
    _:sentence:_:xs <- C.getContents <&> (C.lines >>> map C.words)

    let (r,w) = foldr (\case
                        [x,y,"correct"  ] -> first  (M.insertWith (<>) x [y])
                        [x,y,"incorrect"] -> second (M.insertWith (<>) x [y])
                    ) (M.empty,M.empty) xs
        (a,b) = case translations (r,w) (1,1) sentence of
            (1,_) -> (translate r sentence, "correct")
            (_,1) -> (translate (r<>w) sentence, "incorrect")
            (x,y) -> (C.pack (show x) <> " correct", C.pack (show (y-x)) <> " incorrect")

    C.putStrLn a
    C.putStrLn b

translate :: M.Map C.ByteString [C.ByteString] -> [C.ByteString] -> C.ByteString
translate m = map ((m M.!) >>> head) >>> C.unwords

translations :: (M.Map C.ByteString [C.ByteString], M.Map C.ByteString [C.ByteString]) -> (Int,Int) -> [C.ByteString] -> (Int,Int)
translations _ (r,w) [] = (r,w)
translations (correct,incorrect) (r,w) (x:xs) = translations (correct,incorrect) (r*f correct, w*(f correct + f incorrect)) xs
    where f = M.findWithDefault [] x >>> length
