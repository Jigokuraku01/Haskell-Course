import Data.Functor
import Data.List ()

type Symb = String

infixl 2 :@

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Read, Show)

subst :: Symb -> Expr -> Expr -> Expr
subst s expr = helper
  where
    fvs = freeVars expr
    exprFreeVars = freeVars expr
    helper (Var x)
      | x == s = expr
      | otherwise = Var x
    helper (expr1 :@ expr2) = helper expr1 :@ helper expr2
    helper (Lam x inp_expr)
      | s == x = Lam x inp_expr
      | x `notElem` fvs = Lam x (helper inp_expr)
      | otherwise =
          let x' = findFreshName x fvs (freeVars inp_expr)
           in Lam x' (subst s expr (renameVar x x' inp_expr))

findFreshName :: Symb -> [Symb] -> [Symb] -> Symb
findFreshName base used1 used2 =
  head [name | name <- names, name `notElem` allUsed]
  where
    allUsed = used1 ++ used2
    names = base : [base ++ "_" ++ show i | i <- [1 ..]]

renameVar :: Symb -> Symb -> Expr -> Expr
renameVar x y (Var z)
  | z == x = Var y
  | otherwise = Var z
renameVar x y (t1 :@ t2) = renameVar x y t1 :@ renameVar x y t2
renameVar x y (Lam s term)
  | x == s = Lam y (renameVar x y term)
  | otherwise = Lam s (renameVar x y term)

replaceSymb :: Symb -> Symb -> Expr -> Expr
replaceSymb wrong_symb new_symb (Var x)
  | x == wrong_symb = Var new_symb
  | otherwise = Var x
replaceSymb wrong_symb new_symb (t1 :@ t2) =
  replaceSymb wrong_symb new_symb t1 :@ replaceSymb wrong_symb new_symb t2
replaceSymb wrong_symb new_symb (Lam s term)
  | s == wrong_symb = Lam new_symb (replaceSymb wrong_symb new_symb term)
  | otherwise = Lam s (replaceSymb wrong_symb new_symb term)

freeVars :: Expr -> [Symb]
freeVars = helper []
  where
    helper acc (Var x) =
      if x `elem` acc
        then acc
        else x : acc
    helper acc (e1 :@ e2) = helper (helper acc e1) e2
    helper acc (Lam x body) = filter (/= x) (helper acc body)

infix 1 `alphaEq`

alphaEq :: Expr -> Expr -> Bool
alphaEq = helper []
  where
    helper acc (Var x) (Var y) =
      case lookup x acc of
        Just y' -> y' == y
        Nothing -> x == y
    helper acc (t1_left :@ t2_left) (t1_right :@ t2_right) =
      helper acc t1_left t1_right && helper acc t2_left t2_right
    helper acc (Lam x inp_expr_left) (Lam y inp_expr_right) =
      helper ((x, y) : acc) inp_expr_left inp_expr_right
    helper _ _ _ = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce ((Lam x inp_expr) :@ new_expr) = Just (subst x new_expr inp_expr)
reduceOnce (Lam x inp_expr) = fmap (Lam x) (reduceOnce inp_expr)
reduceOnce (t1 :@ t2) =
  case reduceOnce t1 of
    Just t1' -> Just (t1' :@ t2)
    Nothing -> fmap (t1 :@) (reduceOnce t2)
reduceOnce _ = Nothing

nf :: Expr -> Expr
nf e = maybe e nf (reduceOnce e)

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool
betaEq left right = t1 `alphaEq` t2
  where
    t1 = nf left
    t2 = nf right
