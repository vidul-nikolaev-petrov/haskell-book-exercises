module Main where

vowels :: String
vowels = "aeiou"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . replaceThe . words
  where
    replaceThe (x : xs)
      | null xs = [x]
      | notThe x == Nothing = "a" : replaceThe xs
      | notThe x == Just x = x : replaceThe xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where
    count [] = 0
    count [x] = 0
    count (x : y : xs)
      | notThe x == Nothing && isVowel (head y) = 1 + count xs
      | otherwise = count (y : xs)

countVowels :: String -> Integer
countVowels = count . words
  where
    count [] = 0
    count (x : xs) = vowels x + count xs
      where
        vowels [] = 0
        vowels (x : xs)
          | isVowel x = 1 + vowels xs
          | otherwise = vowels xs

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | (consonants - vowels) >= 0 = Just (Word' s)
  | otherwise = Nothing
  where
    vowels = length $ filter isVowel s
    consonants = length s - vowels

main :: IO ()
main = do
  print $ replaceThe "the sky is the blue"
  print $ replaceThe "a step more to the limit"
  print $ countTheBeforeVowel "the evil blue in the eye"
  print $ countTheBeforeVowel "this is the nothing in the rest"
  print $ countTheBeforeVowel "the blue city and the open window"
  print $ countVowels "the cow"
  print $ countVowels "Mikolajczak"
  print $ mkWord "good"
  print $ mkWord "auh"
