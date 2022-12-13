import Data.List.Split (splitWhen)
import Data.Char (isDigit)
import Data.Bool (bool)
import Data.List (elemIndex)
import Control.Applicative

data Distress = N Int | L [Distress]
    deriving (Eq, Read)

instance Ord Distress where
    (<=) x y | x == a && y == b = False
    (<=) (N a) (N b) = a <= b
    (<=) (L a) (L b) = a <= b
    (<=) (L a) (N b) = L    a  <= L [N b]
    (<=) (N a) (L b) = L [N a] <= L    b

main :: IO ()
main = do
    inn <- fmap (map (map parse) . splitWhen null . lines) (readFile "day13-input.txt")
    print $ sum $ zipWith (\i [a,b] -> bool 0 i (a<=b)) [1..] inn
    print (search 2 (concat inn) * (search 6 (concat inn) + 1))

search :: Int -> [Distress] -> Int
search t xs = 1 + length (filter (N t < ) xs)

parse :: String -> Distress
parse = read . insert

insert :: String -> String
insert [] = []
insert ('[':xs) = "L [" ++ insert xs
insert (x:xs) | isDigit x = "N " ++ [x] ++ takeWhile isDigit xs ++ insert (dropWhile isDigit xs)
              | otherwise = x : insert xs

a = parse "[[5,[[0],6,[7,8,7,5],[4,8,7,7],10],[0,9,[4,9,9,6,3],[6],4],[[8,9],3,[]]],[[]],[0,[[10,10,5,8,5],2,7,0,[3]],[2,[4,6,5,1,6],[7,10,10,4],7]]]"
b = parse "[[5,0],[[[6,3,5],[3],[8,1,5],5,9],[6],[2,0],2,[10]],[]]"
