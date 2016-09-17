module Logger
  ( Logger
  -- ^ Export the data type, not the data constructor.
  , getVal
  , getLog
  , logMsg
  , logWithValue
  ) where

import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Sequence (Seq, empty, singleton)

----------------------------------
--   Data Types and Instances   --
----------------------------------

-- Notice that all instances for the Logger are quite general, i.e. any Monoid
-- will suffice for the log, so if you'd rather not use a Seq, you can change it
-- to any monoid and all instances will still work. The functions later on (and
-- therefore any functions that call them) are not quite as general and will
-- need tweaking.

-- | Logger type, uses Sequences for O(1) value appending and O(log(min(n1,n2)))
--   concatenation, where n1 and n2 are the lengths of the sequences.
newtype Logger a = Logger { runLogger :: (a, Seq String) }

instance Functor Logger where
  fmap f (Logger (x, l)) = Logger (f x, l)

instance Applicative Logger where
  pure a = Logger (a, mempty)
  Logger (f, l) <*> Logger (x, l') = Logger (f x, l <> l')

instance Monad Logger where
  Logger (v, l) >>= f = Logger (id, l) <*> f v

instance Show a => Show (Logger a) where
  show = ppLog

-------------------
--   Functions   --
-------------------

-- | Logs the supplied message in the Logger. Has no effect on values.
logMsg :: String -> Logger ()
logMsg m = Logger ((), singleton m)

-- | Logs the supplied message in the Logger and updates the value.
logWithValue :: String -> a -> Logger a
logWithValue m v = Logger (v, singleton m)

-- | Gets the current value of a Logger.
getVal :: Logger a -> a
getVal = fst . runLogger

-- | Gets the current log of a Logger.
getLog :: Logger a -> Seq String
getLog = snd . runLogger

-- | Pretty prints the log, as well as the final value.
ppLog :: Show a => Logger a -> String
ppLog (Logger (v,l)) = "Log:\n"  <> unlines (map (" -> " <>) (toList l))
                    <> "Value: " <> show v
