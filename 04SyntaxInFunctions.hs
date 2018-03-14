-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit x
            | x == 0 = "Zero"
            | x == 1 = "One"
            | x == 2 = "Two"
            | x == 3 = "Three"
            | x == 4 = "Four"
            | x == 5 = "Five"
            | x == 6 = "Six"
            | x == 7 = "Seven"
            | x == 8 = "Eight"
            | x == 9 = "Nine"
            | otherwise = "Unknown"

-- given a tuple, divide fst by snd, using pattern matching.
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, y)
          | y == 0 = undefined
          | otherwise = x/y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList l
              | length l <= 3 = False
threeZeroList (a:b:c:l)
              | a /= 0 = False
              | b /= 0 = False
              | c /= 0 = False
              | otherwise = True