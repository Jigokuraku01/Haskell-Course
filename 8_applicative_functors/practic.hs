{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

-- Classwork
import Control.Applicative (ZipList (ZipList), getZipList)

-- Task 1
data E l r
  = L l
  | R r
  deriving (Eq, Show)

instance Functor (E l) where
  fmap :: (a -> b) -> E l a -> E l b
  fmap _ (L l) = L l
  fmap f (R r) = R (f r)

instance Applicative (E l) where
  pure :: a -> E l a
  pure = R
  (<*>) :: E l (a -> b) -> E l a -> E l b
  L e <*> _ = L e
  _ <*> L e = L e
  R f <*> R x = R (f x)

-- Homework
-- Task 1
infixl 5 >$<

infixl 5 >*<

(>$<) :: (a -> b) -> [a] -> [b]
f >$< xs = getZipList (f <$> ZipList xs)

(>*<) :: [a -> b] -> [a] -> [b]
fs >*< xs = getZipList (ZipList fs <*> ZipList xs)

-- Task 2
data Triple a
  = Tr a a a
  deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure x = Tr x x x
  (<*>) (Tr f g h) (Tr a b c) = Tr (f a) (g b) (h c)

-- Task 3
data Tree a
  = Nil
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch left val right) = Branch (fmap f left) (f val) (fmap f right)

instance Applicative Tree where
  pure x = Branch (pure x) x (pure x)
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Branch l1 f r1 <*> Branch l2 v r2 = Branch (l1 <*> l2) (f v) (r1 <*> r2)

-- Task 4
newtype Cmps f g x = Cmps
  { getCmps :: f (g x)
  }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps fgx) = Cmps (fmap (fmap f) fgx)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure x = Cmps (pure (pure x))
  Cmps fs <*> Cmps xs = Cmps ((<*>) <$> fs <*> xs)

-- Task 5
divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' [] = ("1.0", 1)
divideList' (x : xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs

-- Task 6
newtype Arr2 e1 e2 a = Arr2
  { getArr2 :: e1 -> e2 -> a
  }

newtype Arr3 e1 e2 e3 a = Arr3
  { getArr3 :: e1 -> e2 -> e3 -> a
  }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 (\x y -> f (g x y))

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 (\x y z -> f (g x y z))

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 (\_ _ -> x)
  Arr2 f <*> Arr2 g = Arr2 (\x y -> f x y (g x y))

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 (\_ _ _ -> x)
  Arr3 f <*> Arr3 g = Arr3 (\x y z -> f x y z (g x y z))
