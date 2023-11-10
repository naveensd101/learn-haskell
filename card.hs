toDigits :: Integer -> [Integer]
toDigits n
    | 0 == n = []
    | n < 0  = []
    | otherwise = (toDigits (div n 10)) ++ ((mod n 10): [])

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | 0 == n = []
    | n < 0  = []
    | otherwise = mod n 10 : toDigitsRev (div n 10)
