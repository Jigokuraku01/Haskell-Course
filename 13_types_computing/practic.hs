{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-z #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- нужно для проверки, не убирайте!

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Except (MonadError, throwError)
import Data.List (group, nub, union)

infixl 4 :@

infixr 3 :->

type Symb = String

-- Терм
data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Show)

-- Тип
data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)

-- Контекст
newtype Env = Env [(Symb, Type)]
  deriving (Eq, Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq, Show)

freeVars :: Expr -> [Symb]
freeVars (Var x) = [x]
freeVars (e1 :@ e2) = nub (freeVars e1 `union` freeVars e2)
freeVars (Lam x e) = filter (/= x) (freeVars e)

freeTVars :: Type -> [Symb]
freeTVars (TVar x) = [x]
freeTVars (t1 :-> t2) = nub (freeTVars t1 `union` freeTVars t2)

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) x t = Env ((x, t) : env)

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = do
  (_, t) <- env
  freeTVars t

fresh :: [Symb] -> Symb
fresh used = head $ filter (`notElem` used) candidates
  where
    candidates = [c : suffix n | n <- [(0 :: Int) ..], c <- ['a' .. 'z']]
    suffix 0 = ""
    suffix n = show n

freshT :: (MonadError String m) => Env -> m Type
freshT env = return (TVar (fresh (freeTVarsEnv env)))

appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env xs) v =
  case lookup v xs of
    Just t -> return t
    Nothing ->
      throwError ("There is no variable \"" ++ v ++ "\" in the environment.")

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy subs) (TVar x) =
  case lookup x subs of
    Just t -> t
    Nothing -> TVar x
appSubsTy subs (t1 :-> t2) = appSubsTy subs t1 :-> appSubsTy subs t2

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv subs (Env env) = Env [(x, appSubsTy subs t) | (x, t) <- env]

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy s1) (SubsTy s2) =
  SubsTy ([(x, appSubsTy (SubsTy s1) t) | (x, t) <- s2] `union` s1)

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []
  mappend = (<>)

unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify (TVar x) (TVar y)
  | x == y = return mempty
unify (TVar x) t2
  | x `notElem` freeTVars t2 = return (SubsTy [(x, t2)])
  | otherwise =
      throwError ("Can't unify " ++ show (TVar x) ++ " with " ++ show t2 ++ "!")
unify t1 (TVar y)
  | y `notElem` freeTVars t1 = return (SubsTy [(y, t1)])
  | otherwise =
      throwError ("Can't unify " ++ show t1 ++ " with " ++ show (TVar y) ++ "!")
unify (t1a :-> t1b) (t2a :-> t2b) = do
  s1 <- unify t1a t2a
  s2 <- unify (appSubsTy s1 t1b) (appSubsTy s1 t2b)
  return (s2 <> s1)

equations :: (MonadError String m) => Env -> Expr -> Type -> m [(Type, Type)]
equations env0 expr t0 = do
  (_, eqs) <- go (freeTVarsEnv env0 ++ freeTVars t0) env0 expr t0
  return eqs
  where
    go used env (Var x) tx = do
      ty <- appEnv env x
      return (used `union` freeTVars ty `union` freeTVars tx, [(ty, tx)])
    go used env (e1 :@ e2) tx = do
      let a = fresh used
      let nt = TVar a
      (used1, eqs1) <- go (a : used) env e1 (nt :-> tx)
      (used2, eqs2) <- go used1 env e2 nt
      return (used1 `union` used2, eqs1 ++ eqs2)
    go used env (Lam x e) tx = do
      let a = fresh used
      let b = fresh (a : used)
      let nta = TVar a
      let ntb = TVar b
      (used1, eqs) <- go (a : b : used) (extendEnv env x nta) e ntb
      return (used1, eqs ++ [(nta :-> ntb, tx)])

generateFreshTypes :: [Symb] -> [(Symb, Type)]
generateFreshTypes vars = go vars []
  where
    go [] _ = []
    go (v : vs) used =
      let tv = fresh used
       in (v, TVar tv) : go vs (tv : used)

principalPair :: (MonadError String m) => Expr -> m (Env, Type)
principalPair expr = do
  let env0 = Env (generateFreshTypes (freeVars expr))
  let used = freeTVarsEnv env0
  let target = TVar (fresh used)
  eqs <- equations env0 expr target
  subs <- foldM step mempty eqs
  let env' = appSubsEnv subs env0
      ty' = appSubsTy subs target
  return (env', ty')
  where
    step acc (t1, t2) = do
      s <- unify (appSubsTy acc t1) (appSubsTy acc t2)
      return (s <> acc)