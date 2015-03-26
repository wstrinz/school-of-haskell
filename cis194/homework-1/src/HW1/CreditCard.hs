module HW1.CreditCard
( toDigits
, toDigitsRev
, doubleEveryOther
, sumDigits
, validate
) where

type CCNumber = Integer

toDigits :: CCNumber -> [Integer]
toDigits cc
  | cc > 0 = map (\x -> read [x] :: Integer) $ show cc
  | otherwise = []

toDigitsRev :: CCNumber -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherHelper . reverse

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper (x:y:xs) = x : 2 * y : doubleEveryOtherHelper xs
doubleEveryOtherHelper a = a


sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: CCNumber -> Bool
validate = (0 ==) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
