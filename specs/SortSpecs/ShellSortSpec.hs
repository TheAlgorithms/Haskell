{-# LANGUAGE ScopedTypeVariables #-}
module SortSpecs.ShellSortSpec where

import Test.Hspec
import Test.QuickCheck
import Sorts.ShellSort

spec :: Spec
spec = do
    describe "shellSort" $ do
        it "returns empty list when sorting empty list" $ property $
            shellSort [] == ([] :: [Int])

        it "returns same list if input was already sorted" $ property $
            \(x :: [Int]) -> shellSort x == (shellSort . shellSort $ x)

        it "returns list with smallest element at 0" $ property $ 
            forAll (listOf1 arbitrary) $
                \(x :: [Int]) -> let sortedList = shellSort x
                        in head sortedList == minimum sortedList

        it "returns list with largest element at the end" $ property $ 
            forAll (listOf1 arbitrary) $
                \(x :: [Int]) -> let sortedList = shellSort x
                        in last sortedList == maximum sortedList

        it "handle simple sorting of static value" $
            let (unsortedList :: [Int]) = [4, 2, 1, 7, 3]
                (sortedList :: [Int]) = [1, 2, 3, 4, 7]
            in shellSort unsortedList == sortedList
