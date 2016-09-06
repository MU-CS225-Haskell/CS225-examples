{-# LANGUAGE DeriveGeneric #-}

-- CSV parsing, vectors and vector algorithms
import Data.Csv
import GHC.Generics
import Data.Vector ((!), (!?), (//))
import qualified Data.Vector as V
import Data.Vector.Algorithms.Merge (sort, sortBy)
import Data.Ord (comparing)

-- For parsing times
import Data.Time.Format
import Data.Time.Clock
import Data.Time.LocalTime

-- Text, with strict and lazy bytestrings for I/O
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Datatype representing a single Sacramento real estate transaction. Best to
--   make each field strict since we'll need to process all the data anyway.
data House = House
  { street    :: !T.Text
  , city      :: !T.Text
  , zip       :: !Int
  , state     :: !T.Text
  , beds      :: !Int
  , baths     :: !Int
  , sqft      :: !Int
  , houseType :: !T.Text
  , saleDate  :: !UTCTime
  , price     :: !Int
  , latitude  :: !Double
  , longitude :: !Double
  } deriving (Show, Generic, Eq, Ord)

-- CSV FromRecord instance. Allows us to easily convert each row into a value
-- of type 'House'. This is automatically derived for us.
instance FromRecord House

-- Let's the CSV library know how it should parse the UTCTime type, should it
-- come across it. parseTimeM accepts a String as its last argument so we need
-- to unpack the ByteString into a String before we can use it here.
instance FromField UTCTime where
  parseField = parseTimeM False defaultTimeLocale "%a %b %d %X %Z %0Y" . BS.unpack

-- | Type restricted decode function
decodeTransactions :: BL.ByteString -> Either String (V.Vector House)
decodeTransactions = decode HasHeader

-- | Averages the house prices
process :: V.Vector House -> Double
process = avg . V.map price

-- | Utilises the Either type's Functor instance to thread any errors through
--   computation in a type safe way.
main :: IO ()
main = do
  -- Read in CSV file as raw ByteString, then decode as a Vector of houses.
  houses <- decodeTransactions <$>
            BL.readFile "resources/Sacramentorealestatetransactions.csv"

  -- Sort the houses, safely (see below). Alternatively, use 'sortByImmutable'
  -- (see below) if you want to sort by a particular category, like house price.
  let sortedHouses = sortImmutable <$> houses

  -- Either print the error or print the top ten houses. It's up to you whether
  -- you want to implement a better Show instance for the House type.
  either print (mapM_ print) $ V.take 10 <$> sortedHouses

---------------
--   UTILS   --
---------------

-- | Sorts the vector in place if it is safe to do so, otherwise it will sort a
--   copy of the vector. This is, overall, a safe operation. See the
--   vector-algorithm and vector docs for more info (search for them on Hackage)
sortImmutable :: Ord a => V.Vector a -> V.Vector a
sortImmutable = V.modify sort

-- | Same as 'sortImmutable' but you can provide your own comparison function.
sortByImmutable :: Ord a => (a -> a -> Ordering) -> V.Vector a -> V.Vector a
sortByImmutable comp = V.modify (sortBy comp)

-- | Generic averaging function.
avg :: (Foldable t, Real a, Fractional b) => t a -> b
avg xs = realToFrac (sum xs) / fromIntegral (length xs)
