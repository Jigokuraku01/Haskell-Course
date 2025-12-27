import Data.Bits (shiftL, (.|.))

-- Task 1
sum3 :: (Num a) => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 l1 l2 l3 = xf + yf + zf : sum3 xs ys zs
  where
    xf = if null l1 then 0 else head l1
    yf = if null l2 then 0 else head l2
    zf = if null l3 then 0 else head l3
    xs = if null l1 then [] else tail l1
    ys = if null l2 then [] else tail l2
    zs = if null l3 then [] else tail l3

-- Task 2-4
digits :: Integer -> [Integer]
digits n = helper n
  where
    helper n
      | n < 0 = helper (-n)
      | n >= 0 && n <= 9 = [n]
      | otherwise = helper (n `div` 10) ++ [n `mod` 10]

containsAllDigits :: Integer -> Bool
containsAllDigits n = helper (digits n) (0 :: Int)
  where
    helper l acc
      | null l =
          acc == (shiftL 1 10 - 2)
      | otherwise =
          helper
            xs
            ( if x /= 0
                then acc .|. shiftL 1 (fromIntegral x)
                else acc
            )
      where
        (x : xs) = l

-- Task 5
sublist n k l = helper n k l 0
  where
    helper n k l cur_index
      | null l = []
      | k <= n = []
      | cur_index >= k = []
      | cur_index < n = helper n k xs (cur_index + 1)
      | otherwise = x : helper n k xs (cur_index + 1)
      where
        (x : xs) = l

-- Task 6
repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem n = concatMap (replicate n)

-- Task 7
movingLists :: Int -> [a] -> [[a]]
movingLists n l
  | length (take n l) < n = []
  | otherwise = take n l : movingLists n (tail l)
