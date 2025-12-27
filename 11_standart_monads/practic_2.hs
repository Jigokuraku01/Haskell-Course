-- Homework
-- Task 1

import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.Writer
import Data.Complex
import Data.IORef
import System.Random

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR acc [] = do
  tell (show acc)
  return acc
minusLoggedR acc (x : xs) = do
  tell "("
  tell (show x)
  tell "-"
  result <- minusLoggedR acc xs
  tell ")"
  return (x - result)

-- Task 2
minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL acc [] = do
  tell (show acc)
  return acc
minusLoggedL initial xs = do
  let (result, log) = foldl buildExpr (initial, "") xs
  tell log
  return result
  where
    buildExpr :: (Show a, Num a) => (a, String) -> a -> (a, String)
    buildExpr (acc, log) x =
      let newAcc = acc - x
          newLog =
            if null log
              then "(" ++ show acc ++ "-" ++ show x ++ ")"
              else "(" ++ log ++ "-" ++ show x ++ ")"
       in (newAcc, newLog)

-- Task 3
fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0, 1)

fibStep :: State (Integer, Integer) ()
fibStep = do
  prev <- get
  let (a, b) = prev
  put (b, a + b)

-- Task 4
while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  val <- readIORef ref
  if p val
    then do
      action
      while ref p action
    else return ()

-- Task 5

avgdevHelper :: Int -> IO Double
avgdevHelper k = do
  i <- newIORef 0
  sumRef <- newIORef 0.0
  while i (< k) $ do
    x <- randomRIO (0.0, 1.0) :: IO Double
    if x < 0.5
      then
        modifyIORef' sumRef (+ 0)
      else
        modifyIORef' sumRef (+ 1)
    modifyIORef' i (+ 1)
  readIORef sumRef

avgdev :: Int -> Int -> IO Double
avgdev k n = do
  ans <- newIORef 0.0
  i <- newIORef 0
  while i (< k) $ do
    dev <- avgdevHelper n
    let tmp = abs (dev - fromIntegral n / 2)
    ans `modifyIORef'` (+ tmp)
    i `modifyIORef'` (+ 1)
  total <- readIORef ans
  return (total / fromIntegral k)

-- Task 6
doOneFlip :: State StdGen Int
doOneFlip = do
  value <- randomRState (0.0, 1.0) :: State StdGen Double
  return $ if value < 0.5 then 0 else 1

seriesOfFlips :: Int -> State StdGen Int
seriesOfFlips n = do
  results <- replicateM n doOneFlip
  return $ sum results

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (x, y) = do
  gen <- get
  let (value, newGen) = randomR (x, y) gen
  put newGen
  return value

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
  flips <- replicateM k (seriesOfFlips n)

  let thN = fromIntegral n / 2
  let dev = map (\x -> abs (fromIntegral x - thN)) flips
  return $ sum dev / fromIntegral k

-- Task 7
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

avgdev'' :: Int -> Int -> Double
avgdev'' k n =
  let get = mkStdGen 42
      randNumb = randomRs (0.0, 1.0) get :: [Double]

      series = take k $ chunks n randNumb

      headCnt = map (length . filter (< 0.5)) series

      dev = map (\x -> abs (fromIntegral x - fromIntegral n / 2)) headCnt
   in sum dev / fromIntegral k
