module LinkedList where

import Data.Monoid ((<>))
import Data.Foldable (toList)

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Read)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  Nil         `mappend` ys = ys
  (Cons x xs) `mappend` ys = Cons x (xs `mappend` ys)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse f Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> ys = (f <$> ys) <> (fs <*> ys)

instance Monad List where
  Nil       >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

-- | Feel free to remove this instance and add a derived one if you'd
--   prefer to see the raw list structure.
instance Show a => Show (List a) where
  show = show . toList

-----------------
--   TESTING   --
-----------------

-- | Converts a foldable structure into a `List`, handy for specifying our
--   list type without having to write `Cons` everywhere.
toLL :: Foldable f => f a -> List a
toLL = foldr Cons Nil

-- | `do` notation test. Returns all possible results of (a + b)^2 with `a`
--   and `b` defined to be any of [1..10] and [2,3,5,7], respectively.
testDo = do
  a <- toLL [1..10]
  b <- toLL [2,3,5,7]
  return $ (a + b)^2

-- | Same idea as above using the applicative style.
testApplicative = f <$> toLL [1,2,3] <*> toLL [4,5,6]
  where f x y = x^2 + y^2

-- Feel free to try out some other functions (making sure to use `toLL`
-- liberally).
