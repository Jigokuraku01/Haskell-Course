import Data.List (unfoldr)

-- Classwork
drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n
  where
    step x g m
      | m > 0 = g (m - 1)
      | otherwise = x : g 0
    ini = const []

-- Homework
-- Task 1

revRange :: (Char, Char) -> [Char]
revRange = unfoldr fun

fun (a, b) = if a > b then Nothing else Just (b, (a, pred b))

-- Task 2
tails' :: [a] -> [[a]]
tails' = foldr fun ini

fun x xs = (x : head xs) : xs

ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'

fun' x acc = [] : map (x :) acc

ini' = [[]]

-- Task 3
reverse' :: [a] -> [a]
reverse' = foldr fun' ini'

fun' x xs = xs ++ [x]

ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''

fun'' xs x = [x] ++ xs

ini'' = []

-- Task 4
infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n

fun x acc n
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = acc (n - 1)

ini = const Nothing

-- Task 5
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v

fun f x g a = g (f a x)

ini = id

-- Task 6
data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

newtype Preorder a
  = PreO (Tree a)
  deriving (Eq, Show)

newtype Postorder a
  = PostO (Tree a)
  deriving (Eq, Show)

newtype Levelorder a
  = LevelO (Tree a)
  deriving (Eq, Show)

instance Foldable Tree where
  foldr _ v Nil = v
  foldr f v (Branch l x r) = foldr f (f x (foldr f v r)) l

instance Foldable Preorder where
  foldr f v (PreO t) = foldrPre f v t
    where
      foldrPre _ acc Nil = acc
      foldrPre func acc (Branch l x r) =
        func x (foldrPre func (foldrPre func acc r) l)

instance Foldable Postorder where
  foldr f v (PostO t) = foldrPost f v t
    where
      foldrPost _ acc Nil = acc
      foldrPost func acc (Branch l x r) =
        foldrPost func (foldrPost func (func x acc) r) l

instance Foldable Levelorder where
  foldr f v (LevelO t) = foldrLevel f v [] [t]
    where
      foldrLevel _ acc cur [] =
        case cur of
          [] -> acc
          _ -> foldrLevel f acc [] (reverse cur)
      foldrLevel func acc next (Nil : xs) = foldrLevel func acc next xs
      foldrLevel func acc next (Branch l x r : xs) =
        func x (foldrLevel func acc (r : l : next) xs)