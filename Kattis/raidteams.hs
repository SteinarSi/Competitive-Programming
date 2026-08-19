import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    players <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> \(x:xs) -> (x, map readInt xs)))

    let xs = map snd (sort (map (\(n,[x,_,_]) -> (-x,n)) players))
        ys = map snd (sort (map (\(n,[_,y,_]) -> (-y,n)) players))
        zs = map snd (sort (map (\(n,[_,_,z]) -> (-z,n)) players))

    distribute S.empty xs ys zs
        & C.unlines
        & C.putStr

distribute :: S.Set C.ByteString -> [C.ByteString] -> [C.ByteString] -> [C.ByteString] -> [C.ByteString]
distribute used [] _  _  = []
distribute used _  [] _  = []
distribute used _  _  [] = []
distribute used (x:xs) (y:ys) (z:zs)
    | S.member x used                     = distribute used xs (y:ys) (z:zs)
    | y == x || S.member y used           = distribute used (x:xs) ys (z:zs)
    | z == x || z == y || S.member z used = distribute used (x:xs) (y:ys) zs
    | otherwise                           = C.unwords (sort [x,y,z]) : distribute (S.insert x (S.insert y (S.insert z used))) xs ys zs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
