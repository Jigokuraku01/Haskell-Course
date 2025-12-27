import Data.List

-- Classwork
-- Tas 1
type Symb = String

infixr 3 :->

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

arity :: Type -> Int
arity (TVar _) = 0
arity (_ :-> y) = 1 + arity y

order :: Type -> Int
order (TVar _) = 0
order (t1 :-> t2) = max (order t1 + 1) (order t2)

-- Task 2
infixl 4 :@

data Term
  = Var Symb
  | Term :@ Term
  | Lam Symb Type Term
  deriving (Eq, Show)

freeVars :: Term -> [Symb]
freeVars (Var x) = [x]
freeVars (t1 :@ t2) = freeVars t1 `union` freeVars t2
freeVars (Lam s _ term) = filter (/= s) (freeVars term)

boundVars :: Term -> [Symb]
boundVars (Var _) = []
boundVars (t1 :@ t2) = boundVars t1 ++ boundVars t2
boundVars (Lam s _ term) = s : boundVars term

-- Task 3
type Env = [(Symb, Type)]

infer0 :: Term -> Maybe Type
infer0 = infer []

infer :: Env -> Term -> Maybe Type
infer env (Var x) =
  case find (\(first, _) -> first == x) env of
    Just (_, typ) -> Just typ
    Nothing -> Nothing
infer env (t1 :@ t2) = do
  firstType <- infer env t1
  secType <- infer env t2
  case firstType of
    (dom :-> cod) | dom == secType -> Just cod
    _ -> Nothing
infer env (Lam s typ term) = do
  bodyType <- infer (env ++ [(s, typ)]) term
  return (typ :-> bodyType)

-- Homework
-- Task 1
data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp first second = compare (to_numb first) (to_numb second)
  where
    to_numb Error = 2
    to_numb Warning = 1
    to_numb Info = 0

-- Task 2
data Person = Person {firstName :: String, lastName :: String, age :: Int}

abbrFirstName :: Person -> Person
abbrFirstName p
  | length (firstName p) < 2 = p
  | otherwise = Person {lastName = lastName p, age = age p, firstName = x : "."}
  where
    (x : xs) = firstName p

-- Task 3
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

tree = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node left value right) = treeSum left + treeSum right + value

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node left _ right) = max (treeHeight left) (treeHeight right) + 1