-- Classwork
-- Task 1
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Identity (Identity (..))
import Control.Monad.State

newtype StrRdrT m a = StrRdrT {runStrRdrT :: String -> m a}

instance (Monad m) => Monad (StrRdrT m) where
  return :: a -> StrRdrT m a
  return = StrRdrT . const . return

  (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
  (StrRdrT f) >>= g = StrRdrT $ \s -> do
    a <- f s
    let StrRdrT h = g a
    h s

instance (MonadFail m) => MonadFail (StrRdrT m) where
  fail :: String -> StrRdrT m a
  fail = StrRdrT . const . fail

instance (Monad m) => Functor (StrRdrT m) where
  fmap :: (Monad m) => (a -> b) -> StrRdrT m a -> StrRdrT m b
  fmap f (StrRdrT g) = StrRdrT $ \s -> fmap f (g s)

instance (Monad m) => Applicative (StrRdrT m) where
  pure :: (Monad m) => a -> StrRdrT m a
  pure = StrRdrT . const . return
  (<*>) :: (Monad m) => StrRdrT m (a -> b) -> StrRdrT m a -> StrRdrT m b
  (StrRdrT f) <*> (StrRdrT g) = StrRdrT $ \s -> do
    h <- f s
    a <- g s
    return (h a)

-- Task 2

--------------------------------------
askStrRdr :: (Monad m) => StrRdrT m String
askStrRdr = StrRdrT return

asksStrRdr :: (Monad m) => (String -> a) -> StrRdrT m a
asksStrRdr f = StrRdrT (return . f)

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr (StrRdrT f) s = runIdentity (f s)

-- Task 3
instance MonadTrans StrRdrT where
  lift :: (Monad m) => m a -> StrRdrT m a
  lift ma = StrRdrT $ const ma

-- Task 4
instance (MonadState s m) => MonadState s (StrRdrT m) where
  get :: (MonadState s m) => StrRdrT m s
  get = lift get
  put :: (MonadState s m) => s -> StrRdrT m ()
  put = lift . put
  state :: (MonadState s m) => (s -> (a, s)) -> StrRdrT m a
  state = lift . state

-- Task 5
class (Monad m) => MonadStrRdr m where
  askSR :: m String
  asksSR :: (String -> a) -> m a
  strRdr :: (String -> a) -> m a

instance (Monad m) => MonadStrRdr (StrRdrT m) where
  askSR :: StrRdrT m String
  askSR = StrRdrT return
  asksSR :: (String -> a) -> StrRdrT m a
  asksSR f = StrRdrT (return . f)
  strRdr :: (String -> a) -> StrRdrT m a
  strRdr f = StrRdrT (return . f)

instance (MonadStrRdr m) => MonadStrRdr (StateT s m) where
  askSR :: StateT s m String
  askSR = lift askSR
  asksSR :: (String -> a) -> StateT s m a
  asksSR f = lift (asksSR f)
  strRdr :: (String -> a) -> StateT s m a
  strRdr f = lift (strRdr f)
