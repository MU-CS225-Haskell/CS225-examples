module Main where

main :: IO ()
main = do
  putStrLn "Second Max:" >> lab02
  putStrLn "Outlier:"    >> lab02'

-- | Reading input for `secondMax`. Deliberately golfed so you can see just
--   how short Haskell code can be.
lab02 :: IO ()
lab02 = map (read :: String -> Int) . words <$> getLine >>= print . secondMax

-- | Reading input for `outlier`. The `maybe` function takes a value of type
--   `Maybe a` and, if it exists, applys a function to it and unwraps the Maybe.
--   Otherwise, it returns a default value (which is "NA" in this case).
lab02' :: IO ()
lab02' = do
  [a,b,c] <- map read . words <$> getLine
  putStrLn $ maybe "NA" show (outlier a b c)

-- | Might take a little bit of thinking but the idea is simple. Assume we have
--   a (sndFromMx, mx) pair, and we come across a new element, v. We want to update
--   mx to be the result of `max mx v`, but we want to update sndFromMx to be the
--   minimum of mx and (max sndFromMx v). This will always give us the second
--   largest value in the list.
secondMax :: Ord a => [a] -> a
secondMax (x:x':xs) = fst $ foldl (\(mx', mx) v -> (min mx (max mx' v), max mx v)) (min x x', max x x') xs
secondMax _ = error "Based on the input constraints, this can't be reached"

-- | This was a little weird, his definition of an outlier is not the
--   statistical definition so I basically just wrote down the spec for the
--   problem. Should make sense. The function can return nothing if there is no
--   outlier, so we return a Maybe Int. Also note the use of `compare` and the
--   `Ordering` data type.
outlier :: Int -> Int -> Int -> Maybe Int
outlier a b c =
  let mx = maximum [a,b,c]
      mn = minimum [a,b,c]
      md = a + b + c - mx - mn
  in case compare (mx - md) (md - mn) of
    GT -> Just mx
    LT -> Just mn
    EQ -> Nothing
