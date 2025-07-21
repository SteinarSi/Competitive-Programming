import           Control.Monad (unless)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    xs <- getLine <&> map (=='A')
    let ks = filter (\k -> wouldHaveWon k (0,0) (0,0) xs) [1 .. length xs]
    print (length ks)
    unless (null ks) (putStrLn (unwords (map show ks)))

wouldHaveWon :: Int -> (Int,Int) -> (Int,Int) -> [Bool] -> Bool
wouldHaveWon k (ga,gb) (pa,pb) xs | pa >= k = wouldHaveWon k (ga+1, gb  ) (0,0) xs
                                  | pb >= k = wouldHaveWon k (ga  , gb+1) (0,0) xs
wouldHaveWon k (ga,gb) (pa,pb) [] = ga > gb
wouldHaveWon k (ga,gb) (pa,pb) (x:xs) | x         = wouldHaveWon k (ga,gb) (pa+1,pb) xs
                                      | otherwise = wouldHaveWon k (ga,gb) (pa,pb+1) xs
