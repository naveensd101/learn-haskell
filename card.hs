toDigits :: Integer -> [Integer]
toDigits n
    | 0 == n = []
    | n < 0  = []
    | otherwise = (toDigits (div n 10)) ++ ((mod n 10): [])

---

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | 0 == n = []
    | n < 0  = []
    | otherwise = mod n 10 : toDigitsRev (div n 10)

---

revList :: [Integer] -> [Integer]
revList [] = []
revList (x:y) = revList y ++ (x : [])

---

doubleFront :: [Integer] -> [Integer]
doubleFront [] = []
doubleFront [x] = [x]
doubleFront (x:y:xs) = (x:2*y:doubleFront xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = revList (doubleFront (revList lst))

---

makeDigitList :: [Integer] -> [Integer]
makeDigitList [] = []
makeDigitList [x] = toDigits x
makeDigitList (x:xs) = toDigits x ++ makeDigitList xs

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumDigits :: [Integer] -> Integer
sumDigits lst = sumList (makeDigitList lst)

---

validate :: Integer -> Bool
validate num
    | sumDigits (doubleEveryOther (toDigits num)) `mod` 10 == 0 = True
    | otherwise = False

