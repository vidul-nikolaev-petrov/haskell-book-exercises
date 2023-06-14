module Main where

import Data.Char (isLower, toUpper)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord t@(x : xs)
  | isLower x = toUpper x : xs
  | otherwise = t

capitalizeParagraph :: String -> String
capitalizeParagraph = capitalizeWord . unwords . capitalize . words
  where
    capitalize [] = []
    capitalize [x] = [x]
    capitalize (x : y : xs)
      | last x == '.' = x : capitalizeWord y : capitalize xs
      | last y == '.' = x : capitalize (y : xs)
      | otherwise = x : y : capitalize xs

main :: IO ()
main = do
  print $ capitalizeWord "Tester"
  print $ capitalizeWord "tester"
  print $ capitalizeParagraph "bla. new blue sky. dom el."