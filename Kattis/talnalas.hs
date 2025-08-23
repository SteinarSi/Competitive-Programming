import           Control.Arrow         ((>>>))
import           Control.Monad         (join)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs@(n:start:goal:_) <- C.getContents <&> (C.lines >>> map readInteger)

    let memo = search n (M.fromList (map (,Nothing) xs), [goal])

    backtrack memo goal start
        & format n
        & C.putStr

format :: Integer -> Maybe [Integer] -> C.ByteString
format n Nothing   = C.pack "Neibb\n"
format n (Just xs) = xs
        & map (show
            >>> C.pack
            >>> (\x -> C.replicate (fromIntegral (fromIntegral n - C.length x)) '0' <> x))
        & (C.pack (show (length xs-1)) :)
        & C.unlines

backtrack :: M.Map Integer (Maybe Integer) -> Integer -> Integer -> Maybe [Integer]
backtrack memo goal curr | goal == curr = Just [goal]
                         | otherwise    = join (M.lookup curr memo) >>= backtrack memo goal <&> (curr:)

search :: Integer -> (M.Map Integer (Maybe Integer), [Integer]) -> M.Map Integer (Maybe Integer)
search n (memo, []) = memo
search n (memo, xs) = search n (foldr move (memo, []) xs)
    where
        move :: Integer -> (M.Map Integer (Maybe Integer), [Integer]) -> (M.Map Integer (Maybe Integer), [Integer])
        move x (mem,ret) = move' mem ret options
            where
                move' :: M.Map Integer (Maybe Integer) -> [Integer] -> [Integer] -> (M.Map Integer (Maybe Integer), [Integer])
                move' m ret []     = (m, ret)
                move' m ret (y:ys) = case M.lookup y m of
                    Just Nothing -> move' (M.insert y (Just x) m) (y:ret) ys
                    _            -> move' m ret ys

                options :: [Integer]
                options = concatMap move [1..n]
                    where
                        move i = let digit = (x `mod` (10^i) - x `mod` (10^(i-1))) `div` (10^(i-1))
                                     hi | digit == 9 = x - 10^(i-1) * 9
                                        | otherwise  = x + 10^(i-1)
                                     lo | digit == 0 = x + 10^(i-1) * 9
                                        | otherwise  = x - 10^(i-1)
                                in  [lo, hi]

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
