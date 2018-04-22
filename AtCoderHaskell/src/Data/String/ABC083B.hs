module Data.String.ABC087B where

import           Data.Char (digitToInt)
import           Data.Ix   (inRange)

main :: IO ()
main = do
  input <- map (read :: String -> Int) . words <$> getLine
  let num = head input
  let min = input !! 1
  let max = last input
  print $ someSums num min max

-- | someSums
-- >>> someSums 20 2 5
-- 84
-- >>> someSums 10 1 2
-- 13
-- >>> someSums 100 4 16
-- 4554
someSums :: Int -> Int -> Int -> Int
someSums n a b =  sum $ filter (someFilter a b) [1..n]

-- | someFilter
-- >>> someFilter 2 5 19
-- False
-- >>> someFilter 2 5 14
-- True
-- >>> someFilter 2 5 15
-- False
-- >>> someFilter 2 5 16
-- False
someFilter :: Int -> Int -> Int -> Bool
someFilter min max num =  inRange (min,max) $ someSum num


-- | someSum
-- >>> someSum 111
-- 3
-- >>> someSum 123
-- 6
-- >>> someSum 999
-- 27
someSum :: Int -> Int
someSum num = sum $ splitInt num


-- | splitInt
-- >>> splitInt 11
-- [1,1]
-- >>> splitInt 1234567890
-- [1,2,3,4,5,6,7,8,9,0]
splitInt :: Int -> [Int]
splitInt num = map digitToInt $ show num

