hannoi :: Int -> Char -> Char -> Char -> [[Char]]
hannoi 1 s f i = [[s, f]]
hannoi n s f i = (hannoi (n-1) s i f) ++ [[s, f]] ++ (hannoi (n-1) i f s)

