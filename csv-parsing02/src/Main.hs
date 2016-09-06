-- CSV parsing and Vector library
import Data.Csv
import Data.Vector ((!), (!?), (//))
import qualified Data.Vector as V

-- Lazy ByteString for I/O, Strict ByteString for storing data.
-- You'll probably want 'text' for when you actually want to process
-- the data.
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Type restricted 'decode'
decodeAny :: HasHeader
          -> BL.ByteString
          -> Either String (V.Vector (V.Vector B.ByteString))
decodeAny = decode

-- | Allows for reading only a certain number of lines from the file.
readLines :: Int -> FilePath -> IO BL.ByteString
readLines n fp = BL.concat . take n . BL.split '\n' <$> BL.readFile fp

-- | This program is capable of decoding any valid CSV file, however the form
--   the contents come in is far less useful, as you can see from the output.
--   You should prefer the method shown in 'csv-parsing-01'.
--
--   We'll only print the first 10 rows. Two ways we can do this are shown
--   below. The first is quite slow since 'decode' forces the evaluation of the
--   whole lazy ByteString, meaning we need to read the whole file in. The
--   second only reads the first 10 lines (using the laziness of BL.ByteString),
--   then decodes that small subset of the original file, which is significantly
--   faster.
main :: IO ()
main = slowMain >> fastMain

-- | Has to decode the entire CSV file before any work can be done.
slowMain :: IO ()
slowMain = do
  putStrLn "Decoding everything, then taking 10 rows:"
  rows <- decodeAny HasHeader <$> BL.readFile "resources/Fielding.csv"
  either print (mapM_ print) $ V.take 10 <$> rows

-- | Only takes the first 10 lines of the file, then decodes that. Since the
--   file has ~170,000 lines, this is a lot faster.
fastMain :: IO ()
fastMain = do
  putStrLn "\nTaking only 10 lines from the file, then decoding:"
  rows <- decodeAny HasHeader <$> readLines 10 "resources/Fielding.csv"
  either print (mapM_ print) rows
