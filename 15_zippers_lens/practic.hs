-- Classwork
-- Task 1
data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

type TreeZ a = (a, CntxT a)

data CntxT a
  = CntxT (Tree a) (Tree a) [(Dir, a, Tree a)]
  deriving (Eq, Show)

data Dir
  = L
  | R
  deriving (Eq, Show)

mktz :: Tree a -> TreeZ a
mktz Empty = error "Cannot buid tree"
mktz (Node l x r) = (x, CntxT l r [])

left :: TreeZ a -> TreeZ a
left (x, CntxT l r ctxs) =
  case l of
    Empty -> error "No subtree"
    Node ll y lr -> (y, CntxT ll lr ((L, x, r) : ctxs))

right :: TreeZ a -> TreeZ a
right (x, CntxT l r ctxs) =
  case r of
    Empty -> error "No subtree"
    Node ll y lr -> (y, CntxT ll lr ((R, x, l) : ctxs))

up :: TreeZ a -> TreeZ a
up (_, CntxT _ _ []) = error "No up"
up (x, CntxT l r ((dir, y, t) : ctsx)) = case dir of
  L -> (y, CntxT (Node l x r) t ctsx)
  R -> (y, CntxT t (Node l x r) ctsx)

untz :: TreeZ a -> Tree a
untz (x, CntxT l r []) = Node l x r
untz tz = untz (up tz)

updTZ :: a -> TreeZ a -> TreeZ a
updTZ y (_, CntxT l r ctxs) = (y, CntxT l r ctxs)