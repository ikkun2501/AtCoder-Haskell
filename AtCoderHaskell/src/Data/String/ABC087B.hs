module Data.String.ABC087B where

main :: IO ()
main = do
  aMax <- readLn :: IO Int
  bMax <- readLn :: IO Int
  cMax <- readLn :: IO Int
  money <- readLn :: IO Int
  print $ length $ coins aMax bMax cMax money

-- |coins
-- >>> length $ coins 2 2 2 100
-- 2
-- >>> length $ coins 5 1 0 150
-- 0
-- >>> length $ coins 30 40 50 6000
-- 213
coins :: Int -> Int -> Int -> Int -> [(Int,Int,Int)]
coins aMax bMax cMax money= [(a,b,c) | a <- [0..aMax] , b <- [0..bMax] , c<-[0..cMax],let aMoney = a * 500;bMoney = b * 100;cMoney = c * 50 ,aMoney + bMoney + cMoney == money]
