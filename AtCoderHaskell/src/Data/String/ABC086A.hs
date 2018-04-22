module Data.String.WelcomeToAtCoder where


import Control.Applicative

main :: IO ()
main = do
    putStrLn =<< show . length . filter (\e -> e == '1') <$>  getLine



