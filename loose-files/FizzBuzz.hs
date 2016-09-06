import Control.Monad (forM_)

-- We're going to demonstrate the difference between idiomatic functional code
-- and the type of code that tends to be written when one brings overly
-- imperative reasoning to a functional language.

-- This was transcribed directly from an example I saw of someone teaching
-- Clojure to newcomers to the language. The example was, unfortunately, quite
-- unidiomatic Clojure, but suffered from the larger problem of trying to make
-- functional code imperative. See this section of the repo for how that code
-- looks: 

-- | Need a few 'main' functions
main :: IO ()
main = do
  putStrLn "=== main0 ==="
  main0
  putStrLn "=== main1 ==="
  main1
  putStrLn "=== main2 ==="
  main2
  putStrLn "=== main3 ==="
  main3

-- | Good
main0 :: IO ()
main0 = readLn >>= mapM_ putStrLn . fizzbuzz

-- | Fine, but understand that this desugars to main0
main1 :: IO ()
main1 = do
  n <- readLn
  mapM_ putStrLn $ fizzbuzz n

-- | Bad
main2 :: IO ()
main2 = do
  n <- readLn
  horribleFizzBuzz n

-- | Much better, but still uses horribleFizzBuzz
main3 :: IO ()
main3 = readLn >>= horribleFizzBuzz

-- | fizzbuzz in idiomatic Haskell. Notice that there is also no I/O involved.
--   This is more of a language agnostic point, though. Don't bake I/O into
--   a function call that doesn't need it.
fizzbuzz :: Int -> [String]
fizzbuzz n = map condition [1..n]
  where condition x
          | x `rem` 15 == 0 = "FizzBuzz"
          | x `rem` 5  == 0 = "Buzz"
          | x `rem` 3  == 0 = "Fizz"
          | otherwise       = show x

-- | fizzbuzz in unidiomatic Haskell. This example shows that, techically
--   speaking, you can write imperative looking code in a purely functional
--   language, but know that 1) you may pay massive performance penalties, and
--   2) you'll be slapped by any programmer worth their salt
horribleFizzBuzz :: Int -> IO ()
horribleFizzBuzz n =
  forM_ [1..n] $ \x ->
    if x `rem` 15 == 0
      then putStrLn "FizzBuzz"
      else
        if x `rem` 5 == 0
          then putStrLn "Buzz"
          else
            if x `rem` 3 == 0
              then putStrLn "Fizz"
              else print x

{- The above translates almost directly to the following C++ code:

void fizzbuzz(int n) {
  for (int i = 1; i <= n; i++) {
    if (x % 15 == 0) {
      std::cout << "FizzBuzz" << std::endl;
    }
    else if (x % 5 == 0) {
      std::cout << "Buzz" << std::endl;
    }
    else if (x % 3 == 0) {
      std::cout << "Fizz" << std::endl;
    }
    else std::cout << i << std::endl;
  }
}

This is perfectly fine to write in C++ (although a lot of people will get angry
that you're baking I/O into a function call). If this isn't the proper way to do
this in C++ then someone please correct me! I don't really know C++ that well.

-}
