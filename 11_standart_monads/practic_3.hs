-- Homework
-- Task 1
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus, guard, mplus, mzero, unless, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Char
import Data.Char (isNumber, isPunctuation)
import Data.Foldable (msum)

data ListIndexError
  = ErrTooLargeIndex Int
  | ErrNegativeIndex
  | OtherErr String
  deriving (Eq, Show)

infixl 9 !!!

(!!!) :: (MonadError ListIndexError m) => [a] -> Int -> m a
xs !!! n = do
  if n < 0
    then throwError ErrNegativeIndex
    else case drop n xs of
      (y : _) -> return y
      [] -> throwError (ErrTooLargeIndex n)

-- Task 2

data Excep a
  = Err String
  | Ok a
  deriving (Eq, Show)

-- тестирование
(?/) :: (MonadError String m) => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

example :: Double -> Double -> Excep String
example x y = action `catchError` return
  where
    action = do
      q <- x ?/ y
      guard (q >= 0)
      if q > 100
        then do
          100 <- return q
          undefined
        else return $ show q

instance Functor Excep where
  fmap _ (Err msg) = Err msg
  fmap f (Ok x) = Ok (f x)

instance Applicative Excep where
  pure = Ok
  (Err msg) <*> _ = Err msg
  _ <*> (Err msg) = Err msg
  (Ok f) <*> (Ok x) = Ok (f x)

instance Monad Excep where
  return = Ok
  (Err msg) >>= _ = Err msg
  (Ok x) >>= f = f x

instance MonadFail Excep where
  fail _ = Err "Monad.fail error."

instance Alternative Excep where
  empty = Err "Alternative.empty error."
  (Err _) <|> r = r
  l <|> _ = l

instance MonadError String Excep where
  throwError = Err
  catchError (Err msg) handler = handler msg
  catchError (Ok x) _ = Ok x

instance MonadPlus Excep where
  mzero = Err "Monad.fail mzero."
  (Err _) `mplus` r = r
  l `mplus` _ = l

-- Task 3

data ParseError = ParseError
  { location :: Int,
    reason :: String
  }

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Integer
parseHex str =
  if all isHexDigit str
    then Right (hexToInteger str)
    else Left (findError str 0)
  where
    hexToInteger :: String -> Integer
    hexToInteger = foldl (\acc x -> acc * 16 + toInteger (digitToInt x)) 0

    findError :: String -> Int -> ParseError
    findError [] _ = ParseError 0 "empty string"
    findError (x : xs) pos
      | isHexDigit x = findError xs (pos + 1)
      | otherwise = ParseError (pos + 1) (x : ": invalid digit")

printError :: ParseError -> ParseMonad String
printError (ParseError loc reason) =
  Right $ "At pos " ++ show loc ++ ": " ++ reason

test s = str
  where
    (Right str) =
      do
        n <- parseHex s
        return $ show n
        `catchError` printError

-- Task 4

newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

instance Semigroup PwdError where
  (PwdError msg1) <> (PwdError msg2) = PwdError (msg1 ++ "; " ++ msg2)

instance Monoid PwdError where
  mempty = PwdError ""
  mappend = (<>)

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  password <- liftIO getLine
  unless (length password >= 8) $ do
    liftIO $ putStrLn "Incorrect input: password is too short!"
    throwError (PwdError "too short")
  unless (any isDigit password) $ do
    liftIO $ putStrLn "Incorrect input: password must contain some digits!"
    throwError (PwdError "no digit")
  unless (any isPunctuation password) $ do
    liftIO $ putStrLn "Incorrect input: password must contain some punctuations!"
    throwError (PwdError "no punctuation")
  return password
