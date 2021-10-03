module Ciphers.CaesarCipher(encrypt,decrypt) where 

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- | Encrypting a string maps each character to the character `n` elements
-- after it in the given alphabet, where `n`is the provided key
encrypt :: String -> Int -> String -> String
encrypt = helper (+)

-- | Decrypting a string maps each character to the character `n` elements
-- before it in the given alphabet, where `n` is the provided key
decrypt :: String -> Int -> String -> String
decrypt = helper (-)

helper :: (Int -> Int -> Int) -> String -> Int -> String -> String
helper _ input 0 _ = input
helper _ input _ [] = input
helper op input key alphabet = map (\x -> fromMaybe x (mappedChar $ elemIndex x alphabet)) input
  where
    len = length alphabet
    mappedChar = fmap (\c -> alphabet !! (c `op` key `mod` len))
