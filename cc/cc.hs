reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList [x, y] = [y, x]
reverseList (x : rest) = reverseList rest ++ [x]

toDigitsReversed :: Integer -> [Integer]
toDigitsReversed n
  | n < 0 = []
  | n < 10 = [n]
  | otherwise = mod n 10 : toDigitsReversed (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsReversed n)

count :: [Integer] -> Integer
count [] = 0
count (a : rest) = 1 + count rest

isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  | otherwise = False

evenDoubler :: [Integer] -> [Integer]
evenDoubler [] = []
evenDoubler [a, b] = [a * 2, b]
evenDoubler (a : b : rest) = a * 2 : b : evenDoubler rest

oddDoubler :: [Integer] -> [Integer]
oddDoubler [a] = [a]
oddDoubler (a : b : rest) = a : b * 2 : oddDoubler rest

everyOtherDoubler :: [Integer] -> [Integer]
everyOtherDoubler (a : rest)
  | isEven (count (a : rest)) = evenDoubler (a : rest)
  | otherwise = oddDoubler (a : rest)

listSumInner :: [Integer] -> Integer
listSumInner [] = 0
listSumInner [a] = a
listSumInner (a : rest) = a + listSumInner rest

listSum :: [Integer] -> Integer
listSum [a] = listSumInner [a]
listSum (a : rest) = listSumInner (toDigits a) + listSum rest

isValidCC :: Integer -> Bool
isValidCC n
  | listSum (everyOtherDoubler (toDigits n)) `mod` 10 == 0 = True
  | otherwise = False

main =
--   print
--     (isValidCC 4012888888881881)
    print
    (isValidCC 4012888888881882)