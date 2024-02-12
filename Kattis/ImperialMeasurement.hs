main :: IO ()
main = do
    [amount, from, _, to] <- fmap words getLine
    print (read amount * factor from / factor to)

factor :: String -> Double
factor = factor' units
    where
        factor' ((u, f):us) x | x `elem` u = f
                              | otherwise  = f * factor' us x

units :: [([String], Double)]
units = [
        (["thou", "th"], 1),
        (["inch", "in"], 1000),
        (["foot", "ft"], 12),
        (["yard", "yd"], 3),
        (["chain", "ch"], 22),
        (["furlong", "fur"], 10),
        (["mile", "mi"], 8),
        (["league", "lea"], 3)
    ]
