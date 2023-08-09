module AsPatterns where

import Data.Char (toUpper)

-- no need for as-patterns
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf (x : xs) seq = elem x seq && isSubseqOf xs seq

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\t@(x : xs) -> (t, toUpper x : xs)) . words

main :: IO ()
main = do
  print $ isSubseqOf "blah" "wboloath"
  print $ isSubseqOf "blah" "wootbla"
  print $ isSubseqOf "blah" "blawhoot"
  print $ capitalizeWords "hello world"