import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (guard)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (inRange)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,_):bans <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    putStrLn $ runST $ do
        banned <- newArray ((1,1),(n,n)) False
        sieve banned bans n [(p,s) | p <- notRandom n, s <- notRandom n]

notRandom :: Int -> [Int]
notRandom n = [n-1,n-3..1] <> [n,n-2..1]

sieve :: forall s. STUArray s (Int,Int) Bool -> [(Int,Int)] -> Int -> [(Int,Int)] -> ST s String
sieve banned bans n = sieve' n >>> (<&> format)
    where
        sieve' :: Int -> [(Int,Int)] -> ST s (Maybe [(Int,Int)])
        sieve' 0 _ = pure (Just [])
        sieve' m [] = pure Nothing
        sieve' m (x@(p,s):xs) = do
                b <- readArray banned x
                if b || (p+p,s+s) `elem` bans
                    then sieve' m xs
                    else do
                        sequence_ $ do
                            (p',s') <- bans
                            let q = (p'-p,s'-s)
                            guard (inRange ((1,1),(n,n)) q)
                            pure (writeArray banned q True)
                        ((x:)<$>) <$> sieve' (m-1) xs

format :: Maybe [(Int,Int)] -> String
format = maybe "NO" (map (\(p,s) -> show p <> " " <> show s)
        >>> ("YES":)
        >>> unlines
        >>> init)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
