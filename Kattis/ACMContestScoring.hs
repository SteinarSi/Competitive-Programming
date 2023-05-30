import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = getContents >>= (\(pts, time) -> putStrLn (show pts ++ " " ++ show time)) . solve S.empty M.empty 0 0 . parse

parse :: String -> [(Int, Char, String)]
parse = map ((\(a:b:c:_) -> (read a, head b, c)) . words) . init . lines

solve :: S.Set Char -> M.Map Char Int -> Int -> Int -> [(Int, Char, String)] -> (Int, Int)
solve s m pts time [] = (pts, time)
solve s m pts time ((t, p, "wrong"):xs) = solve s (M.insertWith (+) p 1 m) pts time xs
solve s m pts time ((t, p, "right"):xs) | S.member p s = solve s m pts time xs
                                        | otherwise = solve (S.insert p s) m (succ pts) (time + t + 20 * M.findWithDefault 0 p m) xs
