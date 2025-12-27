-- Homework
-- Task 1
{-# LANGUAGE FlexibleContexts #-}
-- Homework
-- Task 1
{-# LANGUAGE StandaloneDeriving #-}
-- Homework
-- Task 1
{-# LANGUAGE UndecidableInstances #-}

newtype Fix f = In (f (Fix f))

deriving instance (Show (f (Fix f))) => Show (Fix f)

deriving instance (Eq (f (Fix f))) => Eq (Fix f)

out :: Fix f -> f (Fix f)
out (In x) = x

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Fix f -> a
cata phi (In x) = phi (cata phi <$> x)

type Coalgebra f a = a -> f a

ana :: (Functor f) => Coalgebra f a -> a -> Fix f
ana psi x = In (ana psi <$> psi x)

hylo :: (Functor f) => Algebra f a -> Coalgebra f b -> (b -> a)
hylo phi psi = cata phi . ana psi

data B a
  = Empty
  | Zero a
  | One a
  deriving (Eq, Show)

instance Functor B where
  fmap _ Empty = Empty
  fmap f (Zero x) = Zero (f x)
  fmap f (One x) = One (f x)

type Bin = Fix B

phiB :: B Int -> Int
phiB Empty = 0
phiB (Zero x) = 2 * x
phiB (One x) = 1 + 2 * x

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB n =
  if even n
    then Zero (n `div` 2)
    else One (n `div` 2)

int2bin :: Int -> Bin
int2bin = ana psiB

-- Task 2
data E e = Num Int | Add e e | Mult e e
  deriving (Show, Eq)

type Expr = Fix E

instance Functor E where
  fmap _ (Num n) = Num n
  fmap f (Add e1 e2) = Add (f e1) (f e2)
  fmap f (Mult e1 e2) = Mult (f e1) (f e2)

phiE :: E Int -> Int
phiE (Num n) = n
phiE (Add e1 e2) = e1 + e2
phiE (Mult e1 e2) = e1 * e2

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num n) = show n
phiEShow (Add e1 e2) = "(" ++ e1 ++ "+" ++ e2 ++ ")"
phiEShow (Mult e1 e2) = "(" ++ e1 ++ "*" ++ e2 ++ ")"

-- Task 3

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num n) = showString (show n)
phiEShowS (Add e1 e2) = showString "+ " . e1 . showChar ' ' . e2
phiEShowS (Mult e1 e2) = showString "* " . e1 . showChar ' ' . e2

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num n) = push n
phiE' (Add e1 e2) =
  \s ->
    let s' = e1 s
        s'' = e2 s'
     in add s''
phiE' (Mult e1 e2) =
  \s ->
    let s' = e1 s
        s'' = e2 s'
     in mult s''

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'

-- Task 4
data T a x
  = Leaf
  | Branch x a x

instance Functor (T a) where
  fmap _ Leaf = Leaf
  fmap f (Branch l v r) = Branch (f l) v (f r)

type Tree a = Fix (T a)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch l v r) = l + v + r

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum

-- Task 5
phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch l v r) = l ++ [v] ++ r

tree2listInorder :: Tree a -> [a]
tree2listInorder = cata phiTInorder

psiTBST :: (Ord a) => Coalgebra (T a) [a] -- [a] -> T a [a]
psiTBST [] = Leaf
psiTBST (x : xs) =
  let l = filter (< x) xs
      r = filter (>= x) xs
   in Branch l x r

list2BST :: (Ord a) => [a] -> Tree a
list2BST = ana psiTBST

sortL :: (Ord a) => [a] -> [a]
sortL = hylo phiTInorder psiTBST