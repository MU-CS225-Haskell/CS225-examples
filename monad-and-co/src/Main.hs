import Data.Function (on)

main :: IO ()
main = undefined

-------------------
--   Functions   --
-------------------

(//) :: (Real a, Floating b) => a -> a -> b
(//) = (/) `on` realToFrac

divideBy :: (Real a, Floating b) => a -> a -> Maybe b
divideBy 0 _ = Nothing
divideBy x y = Just $ y // x

safeSqrt :: (Real a, Floating b) => a -> Maybe b
safeSqrt n
  | n < 0     = Nothing
  | otherwise = Just $ sqrt $ realToFrac n

safeMaximum :: (Ord a, Foldable t) => t a -> Maybe a
safeMaximum xs
  | null xs   = Nothing
  | otherwise = Just $ maximum xs

---------------------
--   Combinators   --
---------------------

-- | Vanilla, partial function (can error out on some values)
foo :: (Real a, Floating b, Foldable t) => t a -> b
foo = sqrt . sqrt . (// 3) . (*17) . maximum

-- | Total function (won't error out), combined in the worst way possible
horribleButSafeFoo :: (Real a, Floating b, Foldable t) => t a -> Maybe b
horribleButSafeFoo xs =
  case safeMaximum xs of
    Nothing -> Nothing
    Just x  ->
      case safeSqrt ((x*17) // 3) of
        Nothing -> Nothing
        Just x' -> safeSqrt x'

infixl 1 |>
-- | We'll use this to safely and succinctly combine our functions that return
--   `Maybe`s
(|>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing |> _ = Nothing
Just x  |> f = f x

-- | For regular functions, we need to lift them into a `Maybe` context so they
--   work with our combinator. This is as easy as composing the function with
--   `Just`. It's obviously important to make sure the function you're lifting
--   is total, otherwise this entire exercise is pointless.
lift :: (a -> b) -> a -> Maybe b
lift = (Just .)

-- | Our new and improved safeFoo!
safeFoo :: (Real a, Floating b, Foldable t) => t a -> Maybe b
safeFoo xs = safeMaximum xs
          |> lift (*17)
          |> lift (// 3)
          |> safeSqrt
          |> safeSqrt
