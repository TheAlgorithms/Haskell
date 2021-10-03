module Ciphers.Rot13 where

import Data.Char (isAsciiLower, isAsciiUpper)

-- See https://en.wikipedia.org/wiki/ROT13

-- | "Rotates" each character in a string by 13.
-- Note: This only rotates alphabetic characters, any other character is left
-- as-is
dencrypt :: String -> String
dencrypt = map rotate

rotate :: Char -> Char
rotate c | isAsciiLower c = ([c..'z'] ++ ['a'..'z']) !! 13
         | isAsciiUpper c = ([c..'Z'] ++ ['A'..'Z']) !! 13
         | otherwise = c
