module Data.String.ABC081B where


main :: IO ()
main = do
   len <- readLn :: IO Int
   li <- map (read :: String -> Int) . words <$> getLine
   print $ shiftOnly li len

-- |SHIFT ONLY
--
-- >>> shiftOnly [8] 1
-- 3
-- >>> shiftOnly [4] 1
-- 2
-- >>> shiftOnly [2] 1
-- 1
-- >>> shiftOnly [1] 1
-- 0
--
shiftOnly :: [Int] -> Int -> Int
shiftOnly xs len =  let half = halfMap xs
                    in if (== len) . length . evenFilter $ xs then (+1) $ shiftOnly half len else 0


-- | /2 するマップ
-- >>> halfMap [1,2,3,4,5,6,7,8,9,10]
-- [0,1,1,2,2,3,3,4,4,5]
-- >>> halfMap [2,2,2,2]
-- [1,1,1,1]
-- >>> halfMap [3,3,3,3]
-- [1,1,1,1]
-- >>> halfMap [16]
-- [8]
-- >>> halfMap [8]
-- [4]
-- >>> halfMap [4]
-- [2]
-- >>> halfMap [2]
-- [1]
--
halfMap :: [Int] -> [Int]
halfMap = map (`div` 2)


-- | 偶数のみ残すフィルター
-- >>> evenFilter [1,2,3,4,5,6,7,8,9,10]
-- [2,4,6,8,10]
-- >>> evenFilter [0]
-- [0]
--
evenFilter :: [Int] -> [Int]
evenFilter  = filter (\x -> even x)


