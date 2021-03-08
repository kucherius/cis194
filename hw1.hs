-- Validating Credit Card Numbers

toDigitListRev :: Integer -> [Integer]
toDigitListRev n
  | n == 0    = [0]
  | n < 10    = [n]
  | otherwise = [mod n 10] ++ toDigitListRev (div n 10)


toDigitList :: Integer -> [Integer]
toDigitList n = reverse (toDigitListRev n)


doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond n = [if (mod (fst i) 2 == 0) then (snd i)*2 else snd i | i<-(zip [1..] n)]


sumDigits :: [Integer] -> Integer
sumDigits n
  | null n == True  = 0
  | head n < 10     = head n + sumDigits (tail n)
  | otherwise       = sumDigits (toDigitList (head n)) + sumDigits (tail n)


validate :: Integer -> Bool
validate n
  | mod (sumDigits (doubleEverySecond (toDigitListRev n))) 10 == 0 = True
  | otherwise                                                      = False

-- The Towers of Hanoi
