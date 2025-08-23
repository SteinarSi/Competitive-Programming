import           Control.Applicative   ((<|>))
import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, readArray, writeArray, (!))
import           Data.Array.ST         (MArray (newArray), STUArray,
                                        runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (nub)
import qualified Data.Set              as S

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines >>> drop 1 >>> map C.unpack)

    let letters = S.fromList (concat xs)
        remaining = filter (`S.notMember` letters) ['a'..'z']
        con = contradictions xs

    putStrLn $ if any duplicates xs || S.size letters > 18
        then "0"
        else solulu con (take 18 (S.toList letters <> remaining)) (0,0,0) ("","","")
            & maybe "0" (\(ps,qs,rs) -> unwords [ps,qs,rs])

solulu :: UArray (Char,Char) Bool -> [Char] -> (Int,Int,Int) -> (String,String,String) -> Maybe (String,String,String)
solulu con _ (p,q,r) (ps,qs,rs) | p > 6 || q > 6 || r > 6 = Nothing
solulu con [] _ (ps,qs,rs) = Just (ps,qs,rs)
solulu con (x:xs) (p,q,r) (ps,qs,rs) = [
            (((p+1,q,r),(x:ps,qs,rs)), valid ps),
            (((p,q+1,r),(ps,x:qs,rs)), valid qs),
            (((p,q,r+1),(ps,qs,x:rs)), valid rs)
        ]
        & filter snd
        & foldr (fst >>> uncurry (solulu con xs) >>> (<|>)) Nothing
  where
    valid :: [Char] -> Bool
    valid = all (\y -> not (con ! (x,y)))

    valids = [
            (((p+1,q,r),(x:ps,qs,rs)), valid ps),
            (((p,q+1,r),(ps,x:qs,rs)), valid qs),
            (((p,q,r+1),(ps,qs,x:rs)), valid rs)
        ]
        & filter snd
        & map fst

contradictions :: [String] -> UArray (Char,Char) Bool
contradictions xs = runSTUArray $ do
    con <- newArray (('a','a'),('z','z')) False
    forM_ xs $ \[a,b,c] -> do
        writeArray con (a,b) True
        writeArray con (b,a) True
        writeArray con (a,c) True
        writeArray con (c,a) True
        writeArray con (b,c) True
        writeArray con (c,b) True
    pure con

duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (x:xs) = x `elem` xs || duplicates xs
