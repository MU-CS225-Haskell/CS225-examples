module Main where

-- Requires the `containers` package (you'll see it listed in the .cabal file)
import qualified Data.Map as M

main :: IO ()
main = do
  putStrLn "Closest to average:" >> lab03
  putStrLn "Mode:"               >> lab03'

-- | I/O for closest2avg
lab03 :: IO ()
lab03 = do
  _  <- getLine  -- don't need this line (just gives number of ints in `ns`)
  ns <- map (read :: String -> Double) . words <$> getLine
  print $ closest2avg ns

-- | I/O for mode
lab03' :: IO ()
lab03' = do
  _  <- getLine  -- in fact you'll basically never need this line
  ns <- map (read :: String -> Int) . words <$> getLine
  print $ mode ns

-- | Folds the list up, keeping track of whichever number is the closest to
--   the average.
closest2avg :: (Ord a, Fractional a) => [a] -> a
closest2avg xs = foldl1 (\x y -> if abs (x - a) < abs (y - a) then x else y) xs
  where a = avg xs

-- | Simple averaging function
avg :: Fractional a => [a] -> a
avg xs = sum xs / fromIntegral (length xs)

-- | Finds all modes of a list. I think the original question just wanted you to
--   find one particular mode, but this one does way better than that. It's also
--   quite efficient.
mode :: Ord a => [a] -> [a]
mode xs = M.keys (M.filter (== maximum (M.elems counts)) counts)
  where counts = foldr (\x -> M.insertWith' (+) x (1 :: Int)) M.empty xs
