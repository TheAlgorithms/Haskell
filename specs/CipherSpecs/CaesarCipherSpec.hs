{-# LANGUAGE ScopedTypeVariables #-}
module CipherSpecs.CaesarCipherSpec where

import Test.Hspec
import Test.QuickCheck
import Ciphers.CaesarCipher
import Data.Char (toLower)

spec :: Spec 
spec = do
  describe "encrypt" $ do
    it "does nothing when alphabet is empty" $ property $
      encrypt "hello" 5 [] == "hello"

    it "does nothing when key is 0" $ property $
      encrypt "hello" 0 ['a'..'z'] == "hello"

    it "does not transform characters not found in the provided alphabet" $ property $
      encrypt "h3llo" 1 ['a'..'z'] == "i3mmp"

    describe "decrypt" $ do
      it "returns the original input when given the same key and alphabet as encrypt" $ property $
        forAll arbitrary $
          \(asciiInput :: ASCIIString, key :: Int) -> let input = map toLower $ getASCIIString asciiInput
            in decrypt (encrypt input key ['a'..'z']) key ['a'..'z'] == input
