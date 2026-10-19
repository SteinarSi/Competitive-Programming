import           Control.Arrow      ((>>>))
import           Data.Array.ST      (modifyArray, newArray, runSTUArray)
import           Data.Array.Unboxed (UArray, assocs, listArray, (!))
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Data.List          (sortOn)

main :: IO ()
main = do
    _:k:xs <- getContents <&> words

    let ps@(p:_) = (runSTUArray $ do
            ret <- newArray ('a','z') (0::Int)
            mapM_ (mapM_ (flip (modifyArray ret) succ)) xs
            pure ret)
            & assocs
            & sortOn (snd >>> negate)
            & map fst
            & take (read k)

        f :: UArray Char Char
        f = listArray ('a','z') (map (\x -> if elem x ps then x else p) ['a'..'z'])

    map (map (f!)) xs
        & unwords
        & putStrLn
