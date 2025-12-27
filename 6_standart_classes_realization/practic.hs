import Data.Complex

-- Classwork
-- Task 1
data Tree a = Leaf | Node (Tree a) a (Tree a)

elemTree :: (Eq a) => a -> Tree a -> Bool
elemTree x tree_cur = helper x [tree_cur]
  where
    helper _ [] = False
    helper x (Leaf : xs) = helper x xs
    helper x (Node left y right : xs) = x == y || helper x (xs ++ [left, right])

-- Task 2

instance (Eq a) => Eq (Tree a) where
  (==) tree1 tree2 = helper [tree1] [tree2]
    where
      helper [] [] = True
      helper (Leaf : xs) (Leaf : ys) = helper xs ys
      helper (Node l1 x r1 : xs) (Node l2 y r2 : ys) = x == y && helper (xs ++ [l1, r1]) (ys ++ [l2, r2])
      helper _ _ = False

-- Task 3
instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- Task 4

instance (Show (a)) => Show (Tree a) where
  show Leaf = "{}"
  show (Node l x r) = "<" ++ show l ++ show x ++ show r ++ ">"

-- Task 5
instance (Read a) => Read (Tree a) where
  readsPrec _ [] = []
  readsPrec _ ('{' : '}' : rest) = [(Leaf, rest)]
  readsPrec _ ('<' : rest) = case reads rest of
    [(left_node, rest_left)] -> case reads rest_left of
      [(int_act, central)] ->
        case reads central of
          [(right_node, '>' : away_part)] -> [(Node left_node int_act right_node, away_part)]
          _ -> []
      _ -> []
    _ -> []
  readsPrec _ _ = []

-- Homework
-- Task 1
newtype Matrix a = Matrix [[a]]

instance (Show a) => Show (Matrix a) where
  showsPrec _ (Matrix []) = showString "EMPTY"
  showsPrec _ (Matrix [[]]) = showString "EMPTY"
  showsPrec _ (Matrix rows) = showString (helper rows)
    where
      helper [] = ""
      helper [x] = show x
      helper (x : xs) = show x ++ "\n" ++ helper xs

-- Task 2

newtype Cmplx = Cmplx (Complex Double) deriving (Eq)

instance Show Cmplx where
  showsPrec _ (Cmplx (l :+ r)) = showString (show l ++ sign ++ show (abs r))
    where
      sign
        | r >= 0 = "+i*"
        | otherwise = "-i*"

instance Read Cmplx where
  readsPrec _ str =
    case reads str of
      [(left, central)] ->
        case central of
          '+' : 'i' : '*' : rest ->
            case reads rest of
              [(right, end)] -> [(Cmplx (left :+ right), end)]
              _ -> []
          '-' : 'i' : '*' : rest ->
            case reads rest of
              [(right, end)] -> [(Cmplx (left :+ (-right)), end)]
              _ -> []
          _ -> []
      _ -> []

-- Task 3
class (Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | fromEnum x == fromEnum (maxBound `asTypeOf` x) = minBound
    | otherwise = succ x
  spred :: a -> a
  spred x
    | fromEnum x == fromEnum (minBound `asTypeOf` x) = maxBound
    | otherwise = pred x

-- Task 4
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
  | n < 0 =
      drop (length xs - (abs n `mod` length xs)) xs
        ++ take (length xs - (abs n `mod` length xs)) xs
  | otherwise =
      case splitAt n xs of
        (_, []) -> rotate (n `mod` length xs) xs
        (front, back) -> back ++ front

-- Task 5
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n (x : xs) = map (x :) (comb (n - 1) xs) ++ comb n xs
