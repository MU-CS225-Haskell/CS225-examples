import Control.Monad ((<=<))
import Logger

main :: IO ()
main = print $ logComp [[3,4,5],[1,2,1],[1,2,3,4,5,6]]

---------------------------
--   Logging Functions   --
---------------------------

-- | List head with logging
head' :: Show a => [a] -> Logger a
head' xs = do
  logMsg ("Got head of list: " ++ show xs)
  return (head xs)

-- | Floating point division with logging
(//) :: (Show a, Fractional a) => a -> a -> Logger a
x // y = do
  logMsg ("Divided " ++ show x ++ " by " ++ show y)
  return (x / y)

-- | avg function with logging
avg :: (Foldable t, Show (t a), Real a, Fractional b) => t a -> Logger b
avg xs = do
  logMsg ("Averaged: " ++ show xs)
  return (realToFrac (sum xs) / fromIntegral (length xs))

-------------------------------------
--   Composing logging functions   --
-------------------------------------

-- | Function composition (and thus, composition of logs) is possible, and in
--   fact quite easy. Think of (<=<) as being (.) for functions of type
--   `Monad m => a -> m b`
logComp :: [[Int]] -> Logger Double
logComp = (// 3) <=< avg <=< head'

-- | ... as is the use of do notation.
doLog :: Logger Double
doLog = do
  x <- head' [1,2,3]
  y <- 22 // 7
  z <- avg [2,3,5,7,11]

  logWithValue ("Returning " ++ show x ++ " × " ++ show y ++ " + " ++ show z)
               (x*y + z)
