-- Classwork
data Logged a =
  Logged String a
  deriving (Eq, Show)

instance Functor Logged where
  fmap f (Logged log a) = Logged log (f a)

instance Applicative Logged where
  pure = Logged ""
  (Logged logF f) <*> (Logged logA a) = Logged (logA ++ logF) (f a)

instance Monad Logged where
  return = pure
  (Logged log1 a) >>= f =
    let Logged log2 b = f a
     in Logged (log2 ++ log1) b

write2log :: String -> Logged ()
write2log msg = Logged msg ()

