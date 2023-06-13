module Vignere where

import Data.Char (chr, isLower, isUpper, ord)

setChar :: Bool -> Char -> Char -> Char
setChar flip keyChar char
  | isLower char = shiftChar 'a' char
  | isUpper char = shiftChar 'A' char
  | otherwise = char
  where
    shiftChar base c = chr $ (ord c - ord base + shift) `mod` 26 + ord base
      where
        shift = if flip then ord base - ord keyChar else ord keyChar - ord base

vigenereEncrypt :: String -> String -> String
vigenereEncrypt key = zipWith (setChar True) (cycle key)

vigenereDecrypt :: String -> String -> String
vigenereDecrypt key = zipWith (setChar False) (cycle key)

main :: IO ()
main = do
  print $ vigenereEncrypt "ABCD EF G" "This is a test."
  print $ vigenereDecrypt "ABCD EF G" "Tmmv kt a yivg."