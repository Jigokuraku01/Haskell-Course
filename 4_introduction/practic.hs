import Data.Char (digitToInt)

-- Task 1

doubleFact :: Integer -> Integer
doubleFact n
  | n < 0 = error ("Invalid input")
  | n == 0 = 1
  | n == 1 = 1
  | n == 2 = 2
  | otherwise = n * doubleFact (n - 2)

-- Task 2
seqB :: Integer -> Integer
seqB n = helper !! fromIntegral n
  where
    helper = 1 : 2 : 3 : zipWith (+) (zipWith (-) (tail $ tail helper) (map (2 *) $ tail helper)) (map (3 *) helper)

-- Task 3
fibonacci :: Integer -> Integer
fibonacci n
  | n >= 0 = helper !! fromIntegral n
  | otherwise = let k = -n in if even k then negate (helper !! fromIntegral k) else helper !! fromIntegral k
  where
    helper = 0 : 1 : zipWith (+) (tail helper) helper

-- Task 4
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x =
  let str = show (abs x)
      digit_cnt = length str
      digit_sum = sum (map digitToInt str)
   in (toInteger digit_sum, toInteger digit_cnt)

-- Task 5
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 1 (f b / 2 + f a / 2)
  where
    n = 1000
    h = (b - a) / n

    helper i acc
      | i == n = acc * h
      | otherwise = helper (i + 1) (acc + f (a + h * i))
