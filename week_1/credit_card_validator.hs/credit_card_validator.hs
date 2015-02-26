toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigits x = reverse $ toDigitsRev x

toDigitsRev x
  | x <= 0    = []
  | otherwise = (x `mod` 10):(toDigitsRev (x `div` 10))

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | length xs `mod` 2 == 0 = (x):(doubleEveryOther xs)
  | otherwise              = (x * 2):(doubleEveryOther xs)

sumDigits :: [Integer] -> Integer

sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = (sumDigits $ toDigits x) + sumDigits xs

validate :: Integer -> Bool

validate x = (sumDigits $ doubleEveryOther $ toDigits x) `mod` 2 == 0
