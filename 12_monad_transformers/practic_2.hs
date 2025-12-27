-- Homework
-- Task 1
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative (liftA2)
import Control.Monad.Identity
import Control.Monad.Identity (Identity (..))
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Reader

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

instance (Functor m) => Functor (LoggT m) where
  fmap f (LoggT ma) = LoggT $ fmap (\(Logged log x) -> Logged log (f x)) ma

instance (Applicative m) => Applicative (LoggT m) where
  pure x = LoggT $ pure $ Logged "" x

  LoggT mf <*> LoggT mx =
    LoggT $
      liftA2 (\(Logged log1 f) (Logged log2 x) -> Logged (log2 ++ log1) (f x)) mf mx

instance (Monad m) => Monad (LoggT m) where
  return = pure

  m >>= k = LoggT $ do
    Logged log1 x <- runLoggT m
    Logged log2 y <- runLoggT (k x)
    return $ Logged (log2 ++ log1) y

instance (MonadFail m) => MonadFail (LoggT m) where
  fail msg = LoggT $ fail msg

-- Task 2

write2log :: (Monad m) => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

-- Task 3
instance MonadTrans LoggT where
  lift = LoggT . fmap (Logged "")

-- Task 4
instance (MonadState s m) => MonadState s (LoggT m) where
  get = lift get
  put = lift . put
  state = lift . state

-- Task 5

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

-- Task 6
class (Monad m) => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance (Monad m) => MonadLogg (LoggT m) where
  w2log = write2log
  logg (Logged log x) = do
    w2log log
    return x

instance (MonadLogg m) => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg = lift . logg

instance (MonadLogg m) => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg = lift . logg