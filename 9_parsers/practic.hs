-- Classwork
-- Task 1
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

data Result a = Ok a | Error String
  deriving (Eq, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error msg) = Error msg
  fmap f (Ok x) = Ok (f x)

instance Foldable Result where
  foldMap :: (Monoid m) => (a -> m) -> Result a -> m
  foldMap _ (Error _) = mempty
  foldMap f (Ok x) = f x

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error msg) = pure (Error msg)
  traverse f (Ok x) = fmap Ok (f x)

-- Task 2

data NEList a = Single a | Cons a (NEList a)
  deriving (Eq, Show)

instance Functor NEList where
  fmap :: (a -> b) -> NEList a -> NEList b
  fmap f (Single x) = Single (f x)
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable NEList where
  foldMap :: (Monoid m) => (a -> m) -> NEList a -> m
  foldMap f (Single x) = f x
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Traversable NEList where
  sequenceA :: (Applicative f) => NEList (f a) -> f (NEList a)
  sequenceA (Single x) = fmap Single x
  sequenceA (Cons x xs) = fmap Cons x <*> sequenceA xs

-- Task 3
nat :: Parser Char Int
nat = read <$> some (satisfy isDigit)

-- Homework

-- Task 1
data Triple a = Tr a a a deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure x = Tr x x x
  (<*>) (Tr f g h) (Tr a b c) = Tr (f a) (g b) (h c)

instance Foldable Triple where
  foldl f x (Tr a b c) = f (f (f x a) b) c
  foldMap f (Tr a b c) = f a `mappend` f b `mappend` f c

instance Traversable Triple where
  traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c

-- Task 2
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Branch left val right) = Branch (fmap f left) (f val) (fmap f right)

instance Foldable Tree where
  foldMap _ Nil = mempty
  foldMap f (Branch l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

instance Applicative Tree where
  pure x = Branch (pure x) x (pure x)

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Branch l1 f r1 <*> Branch l2 v r2 = Branch (l1 <*> l2) (f v) (r1 <*> r2)

instance Traversable Tree where
  traverse _ Nil = pure Nil
  traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

-- Task 3
newtype Cmps f g x = Cmps {getCmps :: f (g x)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps fgx) = Cmps (fmap (fmap f) fgx)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure x = Cmps (pure (pure x))
  Cmps fs <*> Cmps xs = Cmps ((<*>) <$> fs <*> xs)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldMap h (Cmps fs) = foldMap (foldMap h) fs

-- Task 4

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps fgx) = Cmps (fmap (fmap f) fgx)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure x = Cmps (pure (pure x))
  Cmps fs <*> Cmps xs = Cmps ((<*>) <$> fs <*> xs)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldMap h (Cmps fs) = foldMap (foldMap h) fs

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
  traverse h (Cmps fs) = Cmps <$> traverse (traverse h) fs

-- Task 5

newtype Parser a = Parser
  { apply :: String -> [(a, String)]
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> [(f v, out) | (v, out) <- p inp]

instance Applicative Parser where
  pure x = Parser $ \inp -> [(x, inp)]
  (<*>) (Parser p1) (Parser p2) =
    Parser $ \inp -> [(f v, out2) | (f, out1) <- p1 inp, (v, out2) <- p2 out1]

instance Alternative Parser where
  empty = Parser $ const []
  (<|>) (Parser p1) (Parser p2) = Parser $ \inp -> p1 inp ++ p2 inp

-- Task 6
class
  (Functor f) =>
  Monoidal f
  where
  unit :: f ()
  (*&*) :: f a -> f b -> f (a, b)

instance Monoidal Maybe where
  unit = Just ()
  (*&*) (Just x) (Just y) = Just (x, y)
  (*&*) _ _ = Nothing

instance (Monoid s) => Monoidal ((,) s) where
  unit = (mempty, ())
  (*&*) (s1, x) (s2, y) = (s1 `mappend` s2, (x, y))

instance Monoidal ((->) e) where
  unit = const ()
  (*&*) f g x = (f x, g x)

-- Task 7
unit' :: (Applicative f) => f ()
unit' = pure ()

pair' :: (Applicative f) => f a -> f b -> f (a, b)
pair' x y = (,) <$> x <*> y

-- Task 8

pure' :: (Monoidal f) => a -> f a
pure' x = fmap (const x) unit

ap' :: (Monoidal f) => f (a -> b) -> f a -> f b
ap' cf ca = fmap (uncurry ($)) (cf *&* ca)