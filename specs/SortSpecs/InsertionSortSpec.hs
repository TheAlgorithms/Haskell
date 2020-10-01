{-# LANGUAGE ScopedTypeVariables #-}
module SortSpecs.InsertionSortSpec where

import Test.Hspec
import Test.QuickCheck
import Sorts.InsertionSort

spec :: Spec
spec = do
    describe "insertionSort" $ do
        it "returns empty list when sorting empty list" $ property $
            insertionSort [] == ([] :: [Int])

        it "returns same list if input was already sorted" $ property $
            \(x :: [Int]) -> insertionSort x == (insertionSort . insertionSort $ x)

        it "returns list with smallest element at 0" $ property $ 
            forAll (listOf1 arbitrary) $
                \(x :: [Int]) -> let sortedList = insertionSort x
                        in head sortedList == minimum sortedList

        it "returns list with largest element at the end" $ property $ 
            forAll (listOf1 arbitrary) $
                \(x :: [Int]) -> let sortedList = insertionSort x
                        in last sortedList == maximum sortedList

        it "handle simple sorting of static value" $
            let (unsortedList :: [Int]) = [4, 2, 1, 7, 3]
                (sortedList :: [Int]) = [1, 2, 3, 4, 7]
            in insertionSort unsortedList == sortedList
