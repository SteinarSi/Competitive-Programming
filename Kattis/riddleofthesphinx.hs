{-# LANGUAGE MultiWayIf #-}
import           Data.Functor ((<&>))
import           System.IO    (hFlush, stdout)
import           Text.Printf  (printf)

main :: IO ()
main = do
    a <- ask (1,0,0)
    b <- ask (0,1,0)
    c <- ask (0,0,1)
    abc <- ask (1,1,1)
    a2b3c <- ask (1,2,3)

    say $ if | a+b+c == abc           -> (a,b,c) -- either nothing or a2b3c is wrong
             | a+2*b+3*c == a2b3c     -> (a,b,c) -- abc is wrong
             | abc + b + 2*c == a2b3c -> (abc-b-c,b,c) -- a is wrong
             | 2*abc - a + c == a2b3c -> (a,abc-a-c,c) -- b is wrong
             | 3*abc-b-2*a == a2b3c   -> (a,b,abc-a-b) -- c is wrong
             | otherwise              -> error "Inconsistent"

say :: (Int,Int,Int) -> IO ()
say (a,b,c) = printf "%d %d %d\n" a b c

ask :: (Int,Int,Int) -> IO Int
ask abc = say abc >> hFlush stdout >> getLine <&> read
