{-
Sorter listen
Tell antall tall i listen
Summer listen.
Til å begynne med er antall skritt lik summen av listen.
Om vi så går ett skritt til høyre, er den nye summen lik den gamle minus antall tall i listen.
Så looper vi igjennom listen, for hvert tall sjekker vi om det er større enn tallet vi er på, og hvis ja endrer vi antall tall 


Det funker ikke :(((

-}

import Data.List (sort)

main :: IO () 
main = do
    s <- readFile "day7-input.txt"
    let crabs = sort (read ("[" ++ s ++ "]"))
    print (loop (0, length crabs) 0 crabs (sum crabs))


loop :: (Int, Int) -> Int -> [Int] -> Int -> Int
loop (before, after) pos xs prev = let eqs = length (takeWhile (pos==) xs)
                                       new = prev + before - after - eqs
                                   in  if new > prev then (pos-1)
                                       else loop (before+eqs, after-eqs) (pos+1) (dropWhile (pos==) xs) new