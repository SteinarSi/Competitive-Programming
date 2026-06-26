import           Control.Arrow ((>>>))
import           Data.Char     (isAlpha, isLower, isUpper)
import           Data.Function ((&))

main :: IO ()
main = do
    xs <- getLine
    let ret = styles
            & filter (snd >>> ($ xs))
            & map fst
    putStr $ if null ret
        then "O nei!\n"
        else unlines ret

styles :: [(String, String -> Bool)]
styles = [
        ("COBOL-CASE", singleSplit '-' >>> maybe False (all (all isUpper))),
        ("MACRO_CASE", singleSplit '_' >>> maybe False (all (all isUpper))),
        ("PascalCase", \(x:xs) -> isUpper x && all isAlpha xs),
        ("SCREAMINGCASE", all isUpper),
        ("Train-Case", singleSplit '_' >>> maybe False (all (\(x:xs) -> isUpper x && all isLower xs))),
        ("camelCase", \(x:xs) -> isLower x && all isAlpha xs),
        ("flatcase", all isLower),
        ("kebab-case", singleSplit '-' >>> maybe False (all (all isLower))),
        ("sPoNgEcAsE", sPoNgEcAsE),
        ("snake_case", singleSplit '_' >>> maybe False (all (all isLower)))
    ]

sPoNgEcAsE :: String -> Bool
sPoNgEcAsE []       = True
sPoNgEcAsE [x]      = isLower x
sPoNgEcAsE (x:y:xs) = isLower x && isUpper y && sPoNgEcAsE xs

singleSplit :: Char -> String -> Maybe [String]
singleSplit _ "" = Just []
singleSplit p (x:xs)
    | p == x    = Nothing
    | otherwise = case span (p/=) xs of
        (ys,[])  -> Just [x:ys]
        (ys,[_]) -> Nothing
        (ys,_:y:zs) | p == y    -> Nothing
                    | otherwise -> ((x:ys):) <$> singleSplit p (y:zs)
