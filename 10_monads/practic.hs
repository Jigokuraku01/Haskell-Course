-- Classwork
-- Task 1
doNTurns :: Int -> Board -> [Board]
doNTurns 0 ini = [ini]
doNTurns n ini = (doNTurns (n - 1) ini) >>= next

-- Homework
-- Task 1
surround :: a -> a -> [a] -> [a]
surround x y zs = do
  let lst = fmap (\z -> [x, z, y]) zs
  concat lst

-- Task 2
lookups :: (Eq k) => k -> [(k, v)] -> [v]
lookups x ys = do
  cur_elem <- ys
  True <- return (fst cur_elem == x)
  return (snd cur_elem)

-- Task 3

factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
  x <- reverse (createList 1 n [])
  True <- return (x * x <= n && n `mod` x == 0)
  return (x, n `div` x)

createList i n acc
  | i * i > n = acc
  | otherwise = createList (i + 1) n (i : acc)

-- Task 4
absDiff :: (Num a) => [a] -> [a]
absDiff xs = do
  (x, y) <- zip xs (tail xs)
  return (abs (x - y))

-- Task 5-7

data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

instance Functor OddC where
  fmap f (Un x) = Un (f x)
  fmap f (Bi x y z) = Bi (f x) (f y) (fmap f z)

instance Applicative OddC where
  (Un f) <*> (Un x) = Un (f x)
  (Bi f g fs) <*> (Un x) = Bi (f x) (g x) (fs <*> Un x)
  (Un f) <*> (Bi x y xs) = Bi (f x) (f y) (Un f <*> xs)
  (Bi f g tl) <*> (Bi x y xs) =
    concatOC
      (Bi (Un f <*> Bi x y xs) (Un g <*> Bi x y xs) (Un (tl <*> Bi x y xs)))
  pure = Un

instance Monad OddC where
  return = pure
  xs >>= k = concatOC (fmap k xs)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi (Un x) (Un y) zs) = Bi x y (concatOC zs)
concatOC (Bi (Un x) (Bi y z ys) zs) = Bi x y (concatOC (Bi (Un z) ys zs))
concatOC (Bi (Bi x y zs) (Un w) xs) = Bi x y (concatOC (Bi zs (Un w) xs))
concatOC (Bi (Bi x y zs) (Bi w g gs) xs) =
  Bi x y (concatOC (Bi zs (Bi w g gs) xs))

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC a b c = concatOC (Bi a b (Un c))

